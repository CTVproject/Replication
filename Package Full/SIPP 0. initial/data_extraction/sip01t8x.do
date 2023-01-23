log using sip01t8x, text replace
set mem 1000m

/*------------------------------------------------

  This program reads the 2001 SIPP Wave 8x Topical Module Data File 
  Note:  This program is distributed under the GNU GPL. See end of
  this file and http://www.gnu.org/licenses/ for details.
  by Jean Roth Thu Jun  1 14:59:42 EDT 2006
  Please report errors to jroth@nber.org
  run with do sip01t8x

----------------------------------------------- */

/* The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  */

local dat_name "sipp01t8x.dat"

/* The following line should contain the path to your output '.dta' file */

local dta_name "sipp01t8x.dta"

/* The following line should contain the path to the data dictionary file */

local dct_name "sip01t8x.dct"

/* The line below does NOT need to be changed */

quietly infile using "${extractcodedir}/`dct_name'", using("`dat_name'") clear

/*------------------------------------------------

  Decimal places have been made explict in the dictionary file.
  Stata resolves a missing value of -1 / # of decimal places as a missing value.

 -----------------------------------------------*/

*Everything below this point, aside from the final save, are value labels

#delimit ;

;
label values spanel   spanel; 
label define spanel  
	1996        "Panel Year"                    
;
label values tfipsst  tfipsst;
label define tfipsst 
	1           "Alabama"                       
	2           "Alaska"                        
	4           "Arizona"                       
	5           "Arkansas"                      
	6           "California"                    
	8           "Colorado"                      
	9           "Connecticut"                   
	10          "Delaware"                      
	11          "DC"                            
	12          "Florida"                       
	13          "Georgia"                       
	15          "Hawaii"                        
	16          "Idaho"                         
	17          "Illinois"                      
	18          "Indiana"                       
	19          "Iowa"                          
	20          "Kansas"                        
	21          "Kentucky"                      
	22          "Louisiana"                     
	24          "Maryland"                      
	25          "Massachusetts"                 
	26          "Michigan"                      
	27          "Minnesota"                     
	28          "Mississippi"                   
	29          "Missouri"                      
	30          "Montana"                       
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
	62          "North Dakota, South Dakota,"   
;
label values eoutcome eoutcome;
label define eoutcome
	201         "Completed interview"           
	203         "Compl. partial- missing data; no"
	207         "Complete partial - TYPE-Z; no" 
	213         "TYPE-A, language problem"      
	215         "TYPE-A, insufficient partial"  
	216         "TYPE-A, no one home (noh)"     
	217         "TYPE-A, temporarily absent (ta)"
	218         "TYPE-A, hh refused"            
	219         "TYPE-A, other occupied (specify)"
	234         "TYPE-B, entire hh institut. or"
	248         "TYPE-C, other (specify)"       
	249         "TYPE-C, sample adjustment"     
	250         "TYPE-C, hh deceased"           
	251         "TYPE-C, moved out of country"  
	252         "TYPE-C, living in armed forces"
	253         "TYPE-C, on active duty in Armed"
	254         "TYPE-C, no one over age 15 years"
	255         "TYPE-C, no Wave 1 persons"     
	260         "TYPE-D, moved address unknown" 
	261         "TYPE-D, moved w/in U.S. but"   
	262         "Merged with another SIPP household"
	270         "Mover, no longer located in same"
	271         "Mover, new address located in" 
	280         "Newly spawned case outside fr's"
;
label values rfid2    rfid2l; 
label define rfid2l  
	0           "Member of related subfamily"   
;
label values epopstat epopstat;
label define epopstat
	1           "Adult (15 years of age or older)"
	2           "Child (Under 15 years of age)" 
;
label values eppintvw eppintvw;
label define eppintvw
	1           "Interview (self)"              
	2           "Interview (proxy)"             
	3           "Noninterview - Type Z"         
	4           "Nonintrvw - pseudo Type Z.  Left"
	5           "Children under 15 during"      
;
label values eppmis4  eppmis4l;
label define eppmis4l
	1           "Interview"                     
	2           "Non-interview"                 
;
label values esex     esex;   
label define esex    
	1           "Male"                          
	2           "Female"                        
;
label values erace    erace;  
label define erace   
	1           "White"                         
	2           "Black"                         
	3           "American Indian, Aleut, or Eskimo"
	4           "Asian or Pacific Islander"     
;
label values eorigin  eorigin;
label define eorigin 
	1           "Canadian"                      
	10          "Polish"                        
	11          "Russian"                       
	12          "Scandinavian"                  
	13          "Scotch-Irish"                  
	14          "Scottish"                      
	15          "Slovak"                        
	16          "Welsh"                         
	17          "Other European"                
	2           "Dutch"                         
	20          "Mexican"                       
	21          "Mexican-American"              
	22          "Chicano"                       
	23          "Puerto Rican"                  
	24          "Cuban"                         
	25          "Central American"              
	26          "South American"                
	27          "Dominican Republic"            
	28          "Other Hispanic"                
	3           "English"                       
	30          "African-American or Afro-American"
	31          "American Indian, Eskimo, or Aleut"
	32          "Arab"                          
	33          "Asian"                         
	34          "Pacific Islander"              
	35          "West Indian"                   
	39          "Another group not listed"      
	4           "French"                        
	40          "American"                      
	5           "French-Canadian"               
	6           "German"                        
	7           "Hungarian"                     
	8           "Irish"                         
	9           "Italian"                       
;
label values wpfinwgt wpfinwgt;
label define wpfinwgt
	0           "0000:999999.9999 .Final person weight"
;
label values errp     errp;   
label define errp    
	1           "Reference person w/ rel. persons"
	10          "Unmarried partner of reference"
	11          "Housemate/roommate"            
	12          "Roomer/boarder"                
	13          "Other non-relative of reference"
	2           "Reference Person w/out rel."   
	3           "Spouse of reference person"    
	4           "Child of reference person"     
	5           "Grandchild of reference person"
	6           "Parent of reference person"    
	7           "Brother/sister of reference person"
	8           "Other relative of reference person"
	9           "Foster child of reference person"
;
label values tage     tage;   
label define tage    
	0           "Less than 1 full year old"     
;
label values ems      ems;    
label define ems     
	1           "Married, spouse present"       
	2           "Married, Spouse absent"        
	3           "Widowed"                       
	4           "Divorced"                      
	5           "Separated"                     
	6           "Never Married"                 
;
label values epnspous epnspous;
label define epnspous
	9999        "Spouse not in hhld or person not"
;
label values epnmom   epnmom; 
label define epnmom  
	9999        "No mother in household"        
;
label values epndad   epndad; 
label define epndad  
	9999        "No father in household"        
;
label values epnguard epnguard;
label define epnguard
	-1          "Not in universe"               
	9999        "Guardian not in household"     
;
label values rdesgpnt rdesgpnt;
label define rdesgpnt
	-1          "Not in universe"               
	1           "Yes"                           
	2           "No"                            
;
label values eeducate eeducate;
label define eeducate
	-1          "Not in universe"               
	31          "Less than 1st grade"           
	32          "1st, 2nd, 3rd or 4th grade"    
	33          "5th or 6th grade"              
	34          "7th or 8th grade"              
	35          "9th grade"                     
	36          "10th grade"                    
	37          "11th grade"                    
	38          "12th grade"                    
	39          "High school graduate - high"   
	40          "Some college but no degree"    
	41          "Diploma or certificate from a" 
	42          "Associate degree in college -" 
	43          "Associate Degree in college -" 
	44          "Bachelors degree (For example:"
	45          "Master's degree (For example: MA,"
	46          "Professional School Degree (For"
	47          "Doctorate degree (For example:"
;
label values eawrunv  eawrunv;
label define eawrunv 
	1           "In universe"                   
	-1          "Not in universe"               
;
label values iinccat  iinccat;
label define iinccat 
	0           "Not answered"                  
	1           "less than  $10,000"            
	2           "$10,000 to $20,000"            
	3           "$20,000 to $30,000"            
	4           "$30,000 to $40,000"            
	5           "$40,000 to $50,000"            
	6           "$50,000 or more"               
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipayn    ipayn;  
label define ipayn   
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipayn2   ipayn2l;
label define ipayn2l 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ihlphire ihlphire;
label define ihlphire
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values ihlptrai ihlptrai;
label define ihlptrai
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values ihlpwage ihlpwage;
label define ihlpwage
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
;
label values itraihyn itraihyn;
label define itraihyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ihlpdk   ihlpdk; 
label define ihlpdk  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ifoodhyn ifoodhyn;
label define ifoodhyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iclothyn iclothyn;
label define iclothyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ihoushyn ihoushyn;
label define ihoushyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsuphyn icsuphyn;
label define icsuphyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iothhyn  iothhyn;
label define iothhyn 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijobhelp ijobhelp;
label define ijobhelp
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ireqinc  ireqinc;
label define ireqinc 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ireqcs   ireqcs; 
label define ireqcs  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ireqpat  ireqpat;
label define ireqpat 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ireqdt   ireqdt; 
label define ireqdt  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhrusu  ijhrusu;
label define ijhrusu 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhint   ijhint; 
label define ijhint  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhdres  ijhdres;
label define ijhdres 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhself  ijhself;
label define ijhself 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhcomp  ijhcomp;
label define ijhcomp 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhcler  ijhcler;
label define ijhcler 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhmach  ijhmach;
label define ijhmach 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhojs   ijhojs; 
label define ijhojs  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhged   ijhged; 
label define ijhged  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhcol   ijhcol; 
label define ijhcol  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhlit   ijhlit; 
label define ijhlit  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhlis   ijhlis; 
label define ijhlis  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijheng   ijheng; 
label define ijheng  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhoth   ijhoth; 
label define ijhoth  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijcomptr ijcomptr;
label define ijcomptr
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhjobyn ijhjobyn;
label define ijhjobyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ijhpayyn ijhpayyn;
label define ijhpayyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values inumpay  inumpay;
label define inumpay 
	0           "Not answered"                  
	1           "Single payment"                
	2           "More than one"                 
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values igasvyn  igasvyn;
label define igasvyn 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values itokyn   itokyn; 
label define itokyn  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icaryn   icaryn; 
label define icaryn  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values irideyn  irideyn;
label define irideyn 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iothtyn  iothtyn;
label define iothtyn 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icctype  icctype;
label define icctype 
	0           "Not answered"                  
	1           "Paid part of the cost"         
	2           "Free child care"               
	3           "Neither"                       
	4           "Both"                          
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccpayyn iccpayyn;
label define iccpayyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccempyn iccempyn;
label define iccempyn
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccwho   iccwho; 
label define iccwho  
	0           "Not answered"                  
	1           "An employer"                   
	2           "A charity"                     
	3           "A relative"                    
	4           "A friend"                      
	5           "Other"                         
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov01 icccov0o;
label define icccov0o
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov02 icccov0k;
label define icccov0k
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov03 icccov0l;
label define icccov0l
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov04 icccov0m;
label define icccov0m
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov05 icccov0n;
label define icccov0n
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov06 icccov0p;
label define icccov0p
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icccov07 icccov0q;
label define icccov0q
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccrel   iccrel; 
label define iccrel  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccdayc  iccdayc;
label define iccdayc 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccsitt  iccsitt;
label define iccsitt 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iccaft   iccaft; 
label define iccaft  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ifavou   ifavou; 
label define ifavou  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ifagroc  ifagroc;
label define ifagroc 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ifameal  ifameal;
label define ifameal 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ifaoth   ifaoth; 
label define ifaoth  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ivouga   ivouga; 
label define ivouga  
	0           "Not answered"                  
	1           "Yes"                           
;
label values ivouhar  ivouhar;
label define ivouhar 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ivoufam  ivoufam;
label define ivoufam 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ivouoth  ivouoth;
label define ivouoth 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ivouthh  ivouthh;
label define ivouthh 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icaga    icaga;  
label define icaga   
	0           "Not answered"                  
	1           "Yes"                           
;
label values icachar  icachar;
label define icachar 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icafam   icafam; 
label define icafam  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icaemp   icaemp; 
label define icaemp  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icaoth   icaoth; 
label define icaoth  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icaothh  icaothh;
label define icaothh 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ihatype  ihatype;
label define ihatype 
	0           "Not answered"                  
	1           "Section 8"                     
	2           "Other rental assistance"       
	3           "Not sure/Don't know"           
	-2          "Refused"                       
;
label values ihatype2 ihatypee;
label define ihatypee
	0           "Not answered"                  
	1           "Section 8"                     
	2           "Other rental assistance"       
	3           "Other housing program"         
	4           "Not sure/Don't know"           
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ihaga    ihaga;  
label define ihaga   
	0           "Not answered"                  
	1           "Yes"                           
;
label values ihahous  ihahous;
label define ihahous 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ihachar  ihachar;
label define ihachar 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ihaoth   ihaoth; 
label define ihaoth  
	0           "Not answered"                  
	1           "Yes"                           
;
label values ihaothh  ihaothh;
label define ihaothh 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashsc  icashsc;
label define icashsc 
	0           "Not answered"                  
	1           "Government agency"             
	2           "Family or friends"             
	3           "Someplace else"                
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashhm  icashhm;
label define icashhm 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashamt icashamt;
label define icashamt
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashal  icashal;
label define icashal 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashuse icashuse;
label define icashuse
	0           "Not answered"                  
	1           "Particular use"                
	2           "Whatever was needed"           
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icashren icashren;
label define icashren
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashfoo icashfoo;
label define icashfoo
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashcs  icashcs;
label define icashcs 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashck  icashck;
label define icashck 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashcar icashcar;
label define icashcar
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashoth icashoth;
label define icashoth
	0           "Not answered"                  
	1           "Yes"                           
;
label values icashohh icashohh;
label define icashohh
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsagen  icsagen;
label define icsagen 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icswelf  icswelf;
label define icswelf 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsoth   icsoth; 
label define icsoth  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsothh  icsothh;
label define icsothh 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icspat   icspat; 
label define icspat  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsabs   icsabs; 
label define icsabs  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icscourt icscourt;
label define icscourt
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icscoll  icscoll;
label define icscoll 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsoth2  icsoth2l;
label define icsoth2l
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsgov   icsgov; 
label define icsgov  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icschar  icschar;
label define icschar 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsfam   icsfam; 
label define icsfam  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icselse  icselse;
label define icselse 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icselsee icselsee;
label define icselsee
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsamt   icsamt; 
label define icsamt  
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsncash icsncash;
label define icsncash
	0           "Not answered"                  
	1           "No cash value"                 
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsearl  icsearl;
label define icsearl 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icspart  icspart;
label define icspart 
	0           "Not answered"                  
	1           "Particular use"                
	2           "Whatever was needed"           
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values icsrent  icsrent;
label define icsrent 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsfood  icsfood;
label define icsfood 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsclos  icsclos;
label define icsclos 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsclok  icsclok;
label define icsclok 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icscar   icscar; 
label define icscar  
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsouse  icsouse;
label define icsouse 
	0           "Not answered"                  
	1           "Yes"                           
;
label values icsousee icsousee;
label define icsousee
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iwftype  iwftype;
label define iwftype 
	0           "Not answered"                  
	1           "A Government organization,"    
	2           "A private, for profit company" 
	3           "Or a non-profit organization," 
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iwforg   iwforg; 
label define iwforg  
	0           "Not answered"                  
	1           "Education"                     
	2           "Social Service"                
	3           "Public Safety"                 
	4           "Recreation"                    
	5           "Health"                        
	6           "Religion"                      
	7           "Or something else?"            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iwfind   iwfind; 
label define iwfind  
	0           "Not answered"                  
	1           "Manufacturing"                 
	2           "Wholesale Trade"               
	3           "Retail Trade"                  
	4           "Service"                       
	5           "or somthing else?"             
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iwfjob   iwfjob; 
label define iwfjob  
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iwfcore  iwfcore;
label define iwfcore 
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	3           "Not Sure"                      
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iinqcomp iinqcomp;
label define iinqcomp
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iinqcomo iinqcomo;
label define iinqcomo
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values iincafdc iincafdc;
label define iincafdc
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincga   iincga; 
label define iincga  
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincssi  iincssi;
label define iincssi 
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincfs   iincfs; 
label define iincfs  
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincmcd  iincmcd;
label define iincmcd 
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincwic  iincwic;
label define iincwic 
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincunem iincunem;
label define iincunem
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincph   iincph; 
label define iincph  
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincener iincener;
label define iincener
	0           "Not answered"                  
	1           "Yes"                           
;
label values iinceduc iinceduc;
label define iinceduc
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincqcc  iincqcc;
label define iincqcc 
	0           "Not answered"                  
	1           "Yes"                           
;
label values iinctran iinctran;
label define iinctran
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincmeal iincmeal;
label define iincmeal
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincoth  iincoth;
label define iincoth 
	0           "Not answered"                  
	1           "Yes"                           
;
label values iincothh iincothh;
label define iincothh
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ininlimt ininlimt;
label define ininlimt
	0           "Not answered"                  
	1           "Yes"                           
;
label values inindn   inindn; 
label define inindn  
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininimm  ininimm;
label define ininimm 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininelig ininelig;
label define ininelig
	0           "Not answered"                  
	1           "Yes"                           
;
label values inindk   inindk; 
label define inindk  
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininhas  ininhas;
label define ininhas 
	0           "Not answered"                  
	1           "Yes"                           
;
label values inintran inintran;
label define inintran
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininchar ininchar;
label define ininchar
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininwort ininwort;
label define ininwort
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininplan ininplan;
label define ininplan
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininavai ininavai;
label define ininavai
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininoth  ininoth;
label define ininoth 
	0           "Not answered"                  
	1           "Yes"                           
;
label values ininothh ininothh;
label define ininothh
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipbredyn ipbredyn;
label define ipbredyn
	0           "Not Answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't Know"                    
;
label values iredincr iredincr;
label define iredincr
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredwkrq iredwkrq;
label define iredwkrq
	0           "not Answered"                  
	1           "Yes"                           
;
label values iredcsrq iredcsrq;
label define iredcsrq
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredinfo iredinfo;
label define iredinfo
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredsign iredsign;
label define iredsign
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredssi  iredssi;
label define iredssi 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredlimt iredlimt;
label define iredlimt
	0           "Not Answered"                  
	1           "Yes"                           
;
label values iredoth  iredoth;
label define iredoth 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ireddk   ireddk; 
label define ireddk  
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipastpub ipastpub;
label define ipastpub
	0           "Not answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipastmon ipastmon;
label define ipastmon
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ipastyr  ipastyr;
label define ipastyr 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values inotlimt inotlimt;
label define inotlimt
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotwkrq inotwkrq;
label define inotwkrq
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotcsrq inotcsrq;
label define inotcsrq
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotinfo inotinfo;
label define inotinfo
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotbank inotbank;
label define inotbank
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inothigh inothigh;
label define inothigh
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotmax  inotmax;
label define inotmax 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotsign inotsign;
label define inotsign
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotmarr inotmarr;
label define inotmarr
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotneed inotneed;
label define inotneed
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotold  inotold;
label define inotold 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inototh  inototh;
label define inototh 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values inotdk   inotdk; 
label define inotdk  
	-3          "End of respondent's entries"   
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
;
label values ielignum ielignum;
label define ielignum
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ieligmon ieligmon;
label define ieligmon
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ieligyr  ieligyr;
label define ieligyr 
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ialwaych ialwaych;
label define ialwaych
	0           "Not Answered"                  
	1           "Yes"                           
	2           "No"                            
	-2          "Refused"                       
	-1          "Don't Know"                    
;
label values ikidwkrq ikidwkrq;
label define ikidwkrq
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidcsrq ikidcsrq;
label define ikidcsrq
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidimmi ikidimmi;
label define ikidimmi
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidinfo ikidinfo;
label define ikidinfo
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidsign ikidsign;
label define ikidsign
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidssi  ikidssi;
label define ikidssi 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidlimt ikidlimt;
label define ikidlimt
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikidoth  ikidoth;
label define ikidoth 
	0           "Not Answered"                  
	1           "Yes"                           
;
label values ikiddk   ikiddk; 
label define ikiddk  
	-3          "End of respondent's entries"   
	-2          "Refused"                       
	-1          "Don't know"                    
	0           "Not answered"                  
;
label values ikdstrtm ikdstrtm;
label define ikdstrtm
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;
label values ikdstrty ikdstrty;
label define ikdstrty
	0           "Not answered"                  
	-2          "Refused"                       
	-1          "Don't know"                    
;

#delimit cr
save `dta_name' , replace

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
