log using sip85fp, text replace
set mem 1000m
*This program reads the 1985 SIPP Full Panel Data File 

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
*by Jean Roth Mon Dec 27 12:23:31 EST 2004
*Please report errors to jroth@nber.org
*run with do sip85fp
*Change output file name/location as desired in the first line of the .dct file
*If you are using a PC, you may need to change the direction of the slashes, as in C:\
*  or "\\Nber\home\data\sipp\1985\sip85fp.dat"
* The following changes in variable names have been made, if necessary:
*      '$' to 'd';            '-' to '_';              '%' to 'p';
*For compatibility with other software, variable label definitions are the
*variable name unless the variable name ends in a digit. 
*'1' -> 'a', '2' -> 'b', '3' -> 'c', ... , '0' -> 'j'
* Note:  Variable names in Stata are case-sensitive
clear
quietly infile using "${extractcodedir}\sip85fp"

*Everything below this point are value labels

#delimit ;

;
label values pp_intv1 pp_intv;
label values pp_intv2 pp_intv;
label values pp_intv3 pp_intv;
label values pp_intv4 pp_intv;
label values pp_intv5 pp_intv;
label values pp_intv6 pp_intv;
label values pp_intv7 pp_intv;
label values pp_intv8 pp_intv;
label define pp_intv 
	0           "NOT APPLICABLE, CHILDREN"      
	1           "INTERVIEW, SELF"               
	2           "INTERVIEW, PROXY"              
	3           "NONINTERVIEW - TYPE Z REFUSAL" 
	4           "NONINTERVIEW - TYPE Z OTHER"   
;
label values pp_mis01 pp_mis; 
label values pp_mis02 pp_mis; 
label values pp_mis03 pp_mis; 
label values pp_mis04 pp_mis; 
label values pp_mis05 pp_mis; 
label values pp_mis06 pp_mis; 
label values pp_mis07 pp_mis; 
label values pp_mis08 pp_mis; 
label values pp_mis09 pp_mis; 
label values pp_mis10 pp_mis; 
label values pp_mis11 pp_mis; 
label values pp_mis12 pp_mis; 
label values pp_mis13 pp_mis; 
label values pp_mis14 pp_mis; 
label values pp_mis15 pp_mis; 
label values pp_mis16 pp_mis; 
label values pp_mis17 pp_mis; 
label values pp_mis18 pp_mis; 
label values pp_mis19 pp_mis; 
label values pp_mis20 pp_mis; 
label values pp_mis21 pp_mis; 
label values pp_mis22 pp_mis; 
label values pp_mis23 pp_mis; 
label values pp_mis24 pp_mis; 
label values pp_mis25 pp_mis; 
label values pp_mis26 pp_mis; 
label values pp_mis27 pp_mis; 
label values pp_mis28 pp_mis; 
label values pp_mis29 pp_mis; 
label values pp_mis30 pp_mis; 
label values pp_mis31 pp_mis; 
label values pp_mis32 pp_mis; 
label define pp_mis  
	0           "NOT MATCHED OR NOT IN SAMPLE"  
	1           "INTERVIEW"                     
	2           "NONINTERVIEW"                  
;
label values reaslef1 reaslef;
label values reaslef2 reaslef;
label values reaslef3 reaslef;
label values reaslef4 reaslef;
label values reaslef5 reaslef;
label values reaslef6 reaslef;
label values reaslef7 reaslef;
label values reaslef8 reaslef;
label define reaslef 
	0           "NOT APPLICABLE OR NOT"         
	1           "LEFT - DECEASED"               
	2           "LEFT - INSTITUTIONALIZED"      
	3           "LEFT - LIVING IN ARMED FORCES" 
	4           "LEFT - MOVED OUTSIDE OF"       
	5           "LEFT - SEPARATION OR DIVORCE"  
	6           "LEFT - PERSON #201 OR GREATER" 
	7           "LEFT - OTHER"                  
	8           "ENTERED MERGED HOUSEHOLD"      
	9           "INTERVIEWED IN PREVIOUS WAVE"  
;
label values mst_rgc  mst_rgc;
label define mst_rgc 
	0           "NOT APPLICABLE FOR COVERAGE"   
;
label values lgthht01 lgthht; 
label values lgthht02 lgthht; 
label values lgthht03 lgthht; 
label values lgthht04 lgthht; 
label values lgthht05 lgthht; 
label values lgthht06 lgthht; 
label values lgthht07 lgthht; 
label values lgthht08 lgthht; 
label values lgthht09 lgthht; 
label values lgthht10 lgthht; 
label values lgthht11 lgthht; 
label values lgthht12 lgthht; 
label values lgthht13 lgthht; 
label values lgthht14 lgthht; 
label values lgthht15 lgthht; 
label values lgthht16 lgthht; 
label values lgthht17 lgthht; 
label values lgthht18 lgthht; 
label values lgthht19 lgthht; 
label values lgthht20 lgthht; 
label values lgthht21 lgthht; 
label values lgthht22 lgthht; 
label values lgthht23 lgthht; 
label values lgthht24 lgthht; 
label values lgthht25 lgthht; 
label values lgthht26 lgthht; 
label values lgthht27 lgthht; 
label values lgthht28 lgthht; 
label values lgthht29 lgthht; 
label values lgthht30 lgthht; 
label values lgthht31 lgthht; 
label values lgthht32 lgthht; 
label define lgthht  
	0           "NA, NOT IN A HOUSEHOLD"        
	1           "MARRIED COUPLE HOUSEHOLD"      
	2           "OTHER FAMILY HOUSEHOLD, MALE"  
	3           "OTHER FAMILY HOUSEHOLD,"       
	4           "NONFAMILY HOUSEHOLD, MALE"     
	5           "NONFAMILY HOUSEHOLD, FEMALE"   
;
label values lgtkey01 lgtkey; 
label values lgtkey02 lgtkey; 
label values lgtkey03 lgtkey; 
label values lgtkey04 lgtkey; 
label values lgtkey05 lgtkey; 
label values lgtkey06 lgtkey; 
label values lgtkey07 lgtkey; 
label values lgtkey08 lgtkey; 
label values lgtkey09 lgtkey; 
label values lgtkey10 lgtkey; 
label values lgtkey11 lgtkey; 
label values lgtkey12 lgtkey; 
label values lgtkey13 lgtkey; 
label values lgtkey14 lgtkey; 
label values lgtkey15 lgtkey; 
label values lgtkey16 lgtkey; 
label values lgtkey17 lgtkey; 
label values lgtkey18 lgtkey; 
label values lgtkey19 lgtkey; 
label values lgtkey20 lgtkey; 
label values lgtkey21 lgtkey; 
label values lgtkey22 lgtkey; 
label values lgtkey23 lgtkey; 
label values lgtkey24 lgtkey; 
label values lgtkey25 lgtkey; 
label values lgtkey26 lgtkey; 
label values lgtkey27 lgtkey; 
label values lgtkey28 lgtkey; 
label values lgtkey29 lgtkey; 
label values lgtkey30 lgtkey; 
label values lgtkey31 lgtkey; 
label values lgtkey32 lgtkey; 
label define lgtkey  
	0           "NOT A KEY PERSON"              
;
label values lgtoth01 lgtoth; 
label values lgtoth02 lgtoth; 
label values lgtoth03 lgtoth; 
label values lgtoth04 lgtoth; 
label values lgtoth05 lgtoth; 
label values lgtoth06 lgtoth; 
label values lgtoth07 lgtoth; 
label values lgtoth08 lgtoth; 
label values lgtoth09 lgtoth; 
label values lgtoth10 lgtoth; 
label values lgtoth11 lgtoth; 
label values lgtoth12 lgtoth; 
label values lgtoth13 lgtoth; 
label values lgtoth14 lgtoth; 
label values lgtoth15 lgtoth; 
label values lgtoth16 lgtoth; 
label values lgtoth17 lgtoth; 
label values lgtoth18 lgtoth; 
label values lgtoth19 lgtoth; 
label values lgtoth20 lgtoth; 
label values lgtoth21 lgtoth; 
label values lgtoth22 lgtoth; 
label values lgtoth23 lgtoth; 
label values lgtoth24 lgtoth; 
label values lgtoth25 lgtoth; 
label values lgtoth26 lgtoth; 
label values lgtoth27 lgtoth; 
label values lgtoth28 lgtoth; 
label values lgtoth29 lgtoth; 
label values lgtoth30 lgtoth; 
label values lgtoth31 lgtoth; 
label values lgtoth32 lgtoth; 
label define lgtoth  
	0           "NOT AN 'OTHER' PERSON IN AN"   
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
	3           "AMERICAN INDIAN, ESKIMO OR"    
	4           "ASIAN OR PACIFIC ISLANDER"     
;
label values ethnicty ethnicty;
label define ethnicty
	1           "GERMAN"                        
	2           "ENGLISH"                       
	3           "IRISH"                         
	4           "FRENCH"                        
	5           "ITALIAN"                       
	6           "SCOTISH"                       
	7           "POLISH"                        
	8           "DUTCH"                         
	9           "SWEDISH"                       
	10          "NORWEGIAN"                     
	11          "RUSSIAN"                       
	12          "UKRANIAN"                      
	13          "WELSH"                         
	14          "MEXICAN-AMERICAN"              
	15          "CHICANO"                       
	16          "MEXICAN"                       
	17          "PUERTO RICAN"                  
	18          "CUBAN"                         
	19          "CENTRAL OR SOUTH AMERICAN"     
	20          "OTHER SPANISH"                 
	21          "AFRO-AMERICAN, BLACK OR NEGRO" 
	30          "ANOTHER GROUP NOT LISTED"      
	39          "DON'T KNOW"                    
;
label values rrp_01   rrp;    
label values rrp_02   rrp;    
label values rrp_03   rrp;    
label values rrp_04   rrp;    
label values rrp_05   rrp;    
label values rrp_06   rrp;    
label values rrp_07   rrp;    
label values rrp_08   rrp;    
label values rrp_09   rrp;    
label values rrp_10   rrp;    
label values rrp_11   rrp;    
label values rrp_12   rrp;    
label values rrp_13   rrp;    
label values rrp_14   rrp;    
label values rrp_15   rrp;    
label values rrp_16   rrp;    
label values rrp_17   rrp;    
label values rrp_18   rrp;    
label values rrp_19   rrp;    
label values rrp_20   rrp;    
label values rrp_21   rrp;    
label values rrp_22   rrp;    
label values rrp_23   rrp;    
label values rrp_24   rrp;    
label values rrp_25   rrp;    
label values rrp_26   rrp;    
label values rrp_27   rrp;    
label values rrp_28   rrp;    
label values rrp_29   rrp;    
label values rrp_30   rrp;    
label values rrp_31   rrp;    
label values rrp_32   rrp;    
label define rrp     
	0           "NOT A SAMPLE PERSON, NONMATCH" 
	1           "HOUSEHOLD REFERENCE PERSON,"   
	2           "HOUSEHOLD REFERENCE PERSON"    
	3           "SPOUSE OF HOUSEHOLD REFERENCE" 
	4           "CHILD OF HOUSEHOLD REFERENCE"  
	5           "OTHER RELATIVE OF HOUSEHOLD"   
	6           "NON-RELATIVE OF HOUSEHOLD"     
	7           "NON-RELATIVE OF HOUSEHOLD"     
;
label values age_01   age;    
label values age_02   age;    
label values age_03   age;    
label values age_04   age;    
label values age_05   age;    
label values age_06   age;    
label values age_07   age;    
label values age_08   age;    
label values age_09   age;    
label values age_10   age;    
label values age_11   age;    
label values age_12   age;    
label values age_13   age;    
label values age_14   age;    
label values age_15   age;    
label values age_16   age;    
label values age_17   age;    
label values age_18   age;    
label values age_19   age;    
label values age_20   age;    
label values age_21   age;    
label values age_22   age;    
label values age_23   age;    
label values age_24   age;    
label values age_25   age;    
label values age_26   age;    
label values age_27   age;    
label values age_28   age;    
label values age_29   age;    
label values age_30   age;    
label values age_31   age;    
label values age_32   age;    
label define age     
	0           "LESS THAN 1 YEAR OR NOT A"     
	85          "85 YEARS OR MORE"              
;
label values ms_01    ms;     
label values ms_02    ms;     
label values ms_03    ms;     
label values ms_04    ms;     
label values ms_05    ms;     
label values ms_06    ms;     
label values ms_07    ms;     
label values ms_08    ms;     
label values ms_09    ms;     
label values ms_10    ms;     
label values ms_11    ms;     
label values ms_12    ms;     
label values ms_13    ms;     
label values ms_14    ms;     
label values ms_15    ms;     
label values ms_16    ms;     
label values ms_17    ms;     
label values ms_18    ms;     
label values ms_19    ms;     
label values ms_20    ms;     
label values ms_21    ms;     
label values ms_22    ms;     
label values ms_23    ms;     
label values ms_24    ms;     
label values ms_25    ms;     
label values ms_26    ms;     
label values ms_27    ms;     
label values ms_28    ms;     
label values ms_29    ms;     
label values ms_30    ms;     
label values ms_31    ms;     
label values ms_32    ms;     
label define ms      
	0           "NOT A SAMPLE PERSON, NONMATCH" 
	1           "MARRIED, SPOUSE PRESENT"       
	2           "MARRIED, SPOUSE ABSENT"        
	3           "WIDOWED"                       
	4           "DIVORCED"                      
	5           "SEPARATED"                     
	6           "NEVER MARRIED"                 
;
label values famtyp01 famtyp; 
label values famtyp02 famtyp; 
label values famtyp03 famtyp; 
label values famtyp04 famtyp; 
label values famtyp05 famtyp; 
label values famtyp06 famtyp; 
label values famtyp07 famtyp; 
label values famtyp08 famtyp; 
label values famtyp09 famtyp; 
label values famtyp10 famtyp; 
label values famtyp11 famtyp; 
label values famtyp12 famtyp; 
label values famtyp13 famtyp; 
label values famtyp14 famtyp; 
label values famtyp15 famtyp; 
label values famtyp16 famtyp; 
label values famtyp17 famtyp; 
label values famtyp18 famtyp; 
label values famtyp19 famtyp; 
label values famtyp20 famtyp; 
label values famtyp21 famtyp; 
label values famtyp22 famtyp; 
label values famtyp23 famtyp; 
label values famtyp24 famtyp; 
label values famtyp25 famtyp; 
label values famtyp26 famtyp; 
label values famtyp27 famtyp; 
label values famtyp28 famtyp; 
label values famtyp29 famtyp; 
label values famtyp30 famtyp; 
label values famtyp31 famtyp; 
label values famtyp32 famtyp; 
label define famtyp  
	0           "PRIMARY FAMILY OR NOT A"       
	1           "SECONDARY INDIVIDUAL, NOT A"   
	2           "UNRELATED SUB, SECONDARY"      
	3           "RELATED SUBFAMILY"             
	4           "PRIMARY INDIVIDUAL"            
;
label values famrel01 famrel; 
label values famrel02 famrel; 
label values famrel03 famrel; 
label values famrel04 famrel; 
label values famrel05 famrel; 
label values famrel06 famrel; 
label values famrel07 famrel; 
label values famrel08 famrel; 
label values famrel09 famrel; 
label values famrel10 famrel; 
label values famrel11 famrel; 
label values famrel12 famrel; 
label values famrel13 famrel; 
label values famrel14 famrel; 
label values famrel15 famrel; 
label values famrel16 famrel; 
label values famrel17 famrel; 
label values famrel18 famrel; 
label values famrel19 famrel; 
label values famrel20 famrel; 
label values famrel21 famrel; 
label values famrel22 famrel; 
label values famrel23 famrel; 
label values famrel24 famrel; 
label values famrel25 famrel; 
label values famrel26 famrel; 
label values famrel27 famrel; 
label values famrel28 famrel; 
label values famrel29 famrel; 
label values famrel30 famrel; 
label values famrel31 famrel; 
label values famrel32 famrel; 
label define famrel  
	0           "NOT APPLICABLE, NOT IN"        
	1           "REFERENCE PERSON OF FAMILY"    
	2           "SPOUSE OF FAMILY REFERENCE"    
	3           "CHILD OF FAMILY REFERENCE"     
	4           "OTHER RELATIVE OF FAMILY"      
;
label values famnum01 famnum; 
label values famnum02 famnum; 
label values famnum03 famnum; 
label values famnum04 famnum; 
label values famnum05 famnum; 
label values famnum06 famnum; 
label values famnum07 famnum; 
label values famnum08 famnum; 
label values famnum09 famnum; 
label values famnum10 famnum; 
label values famnum11 famnum; 
label values famnum12 famnum; 
label values famnum13 famnum; 
label values famnum14 famnum; 
label values famnum15 famnum; 
label values famnum16 famnum; 
label values famnum17 famnum; 
label values famnum18 famnum; 
label values famnum19 famnum; 
label values famnum20 famnum; 
label values famnum21 famnum; 
label values famnum22 famnum; 
label values famnum23 famnum; 
label values famnum24 famnum; 
label values famnum25 famnum; 
label values famnum26 famnum; 
label values famnum27 famnum; 
label values famnum28 famnum; 
label values famnum29 famnum; 
label values famnum30 famnum; 
label values famnum31 famnum; 
label values famnum32 famnum; 
label define famnum  
	0           "NOT APPLICABLE, NOT IN"        
;
label values pnsp_01  pnsp;   
label values pnsp_02  pnsp;   
label values pnsp_03  pnsp;   
label values pnsp_04  pnsp;   
label values pnsp_05  pnsp;   
label values pnsp_06  pnsp;   
label values pnsp_07  pnsp;   
label values pnsp_08  pnsp;   
label values pnsp_09  pnsp;   
label values pnsp_10  pnsp;   
label values pnsp_11  pnsp;   
label values pnsp_12  pnsp;   
label values pnsp_13  pnsp;   
label values pnsp_14  pnsp;   
label values pnsp_15  pnsp;   
label values pnsp_16  pnsp;   
label values pnsp_17  pnsp;   
label values pnsp_18  pnsp;   
label values pnsp_19  pnsp;   
label values pnsp_20  pnsp;   
label values pnsp_21  pnsp;   
label values pnsp_22  pnsp;   
label values pnsp_23  pnsp;   
label values pnsp_24  pnsp;   
label values pnsp_25  pnsp;   
label values pnsp_26  pnsp;   
label values pnsp_27  pnsp;   
label values pnsp_28  pnsp;   
label values pnsp_29  pnsp;   
label values pnsp_30  pnsp;   
label values pnsp_31  pnsp;   
label values pnsp_32  pnsp;   
label define pnsp    
	0           "NOT A SAMPLE PERSON, NONMATCH" 
	999         "NOT APPLICABLE"                
;
label values ent_sp01 ent_sp; 
label values ent_sp02 ent_sp; 
label values ent_sp03 ent_sp; 
label values ent_sp04 ent_sp; 
label values ent_sp05 ent_sp; 
label values ent_sp06 ent_sp; 
label values ent_sp07 ent_sp; 
label values ent_sp08 ent_sp; 
label values ent_sp09 ent_sp; 
label values ent_sp10 ent_sp; 
label values ent_sp11 ent_sp; 
label values ent_sp12 ent_sp; 
label values ent_sp13 ent_sp; 
label values ent_sp14 ent_sp; 
label values ent_sp15 ent_sp; 
label values ent_sp16 ent_sp; 
label values ent_sp17 ent_sp; 
label values ent_sp18 ent_sp; 
label values ent_sp19 ent_sp; 
label values ent_sp20 ent_sp; 
label values ent_sp21 ent_sp; 
label values ent_sp22 ent_sp; 
label values ent_sp23 ent_sp; 
label values ent_sp24 ent_sp; 
label values ent_sp25 ent_sp; 
label values ent_sp26 ent_sp; 
label values ent_sp27 ent_sp; 
label values ent_sp28 ent_sp; 
label values ent_sp29 ent_sp; 
label values ent_sp30 ent_sp; 
label values ent_sp31 ent_sp; 
label values ent_sp32 ent_sp; 
label define ent_sp  
	0           "NOT IN SAMPLE OR NONMATCH"     
	99          "NOT APPLICABLE"                
;
label values pnpt_01  pnpt;   
label values pnpt_02  pnpt;   
label values pnpt_03  pnpt;   
label values pnpt_04  pnpt;   
label values pnpt_05  pnpt;   
label values pnpt_06  pnpt;   
label values pnpt_07  pnpt;   
label values pnpt_08  pnpt;   
label values pnpt_09  pnpt;   
label values pnpt_10  pnpt;   
label values pnpt_11  pnpt;   
label values pnpt_12  pnpt;   
label values pnpt_13  pnpt;   
label values pnpt_14  pnpt;   
label values pnpt_15  pnpt;   
label values pnpt_16  pnpt;   
label values pnpt_17  pnpt;   
label values pnpt_18  pnpt;   
label values pnpt_19  pnpt;   
label values pnpt_20  pnpt;   
label values pnpt_21  pnpt;   
label values pnpt_22  pnpt;   
label values pnpt_23  pnpt;   
label values pnpt_24  pnpt;   
label values pnpt_25  pnpt;   
label values pnpt_26  pnpt;   
label values pnpt_27  pnpt;   
label values pnpt_28  pnpt;   
label values pnpt_29  pnpt;   
label values pnpt_30  pnpt;   
label values pnpt_31  pnpt;   
label values pnpt_32  pnpt;   
label define pnpt    
	0           "NOT A SAMPLE PERSON, NONMATCH" 
	999         "NOT APPLICABLE"                
;
label values ent_pt01 ent_pt; 
label values ent_pt02 ent_pt; 
label values ent_pt03 ent_pt; 
label values ent_pt04 ent_pt; 
label values ent_pt05 ent_pt; 
label values ent_pt06 ent_pt; 
label values ent_pt07 ent_pt; 
label values ent_pt08 ent_pt; 
label values ent_pt09 ent_pt; 
label values ent_pt10 ent_pt; 
label values ent_pt11 ent_pt; 
label values ent_pt12 ent_pt; 
label values ent_pt13 ent_pt; 
label values ent_pt14 ent_pt; 
label values ent_pt15 ent_pt; 
label values ent_pt16 ent_pt; 
label values ent_pt17 ent_pt; 
label values ent_pt18 ent_pt; 
label values ent_pt19 ent_pt; 
label values ent_pt20 ent_pt; 
label values ent_pt21 ent_pt; 
label values ent_pt22 ent_pt; 
label values ent_pt23 ent_pt; 
label values ent_pt24 ent_pt; 
label values ent_pt25 ent_pt; 
label values ent_pt26 ent_pt; 
label values ent_pt27 ent_pt; 
label values ent_pt28 ent_pt; 
label values ent_pt29 ent_pt; 
label values ent_pt30 ent_pt; 
label values ent_pt31 ent_pt; 
label values ent_pt32 ent_pt; 
label define ent_pt  
	0           "NOT IN SAMPLE OR NONMATCH"     
	99          "NOT APPLICABLE"                
;
label values higrade1 higrade;
label values higrade2 higrade;
label values higrade3 higrade;
label values higrade4 higrade;
label values higrade5 higrade;
label values higrade6 higrade;
label values higrade7 higrade;
label values higrade8 higrade;
label define higrade 
	0           "NOT APPLICABLE IF UNDER 15,"   
;
label values grd_cmp1 grd_cmp;
label values grd_cmp2 grd_cmp;
label values grd_cmp3 grd_cmp;
label values grd_cmp4 grd_cmp;
label values grd_cmp5 grd_cmp;
label values grd_cmp6 grd_cmp;
label values grd_cmp7 grd_cmp;
label values grd_cmp8 grd_cmp;
label define grd_cmp 
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values vetstat1 vetstat;
label values vetstat2 vetstat;
label values vetstat3 vetstat;
label values vetstat4 vetstat;
label values vetstat5 vetstat;
label values vetstat6 vetstat;
label values vetstat7 vetstat;
label values vetstat8 vetstat;
label define vetstat 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values in_af_1  in_af;  
label values in_af_2  in_af;  
label values in_af_3  in_af;  
label values in_af_4  in_af;  
label values in_af_5  in_af;  
label values in_af_6  in_af;  
label values in_af_7  in_af;  
label values in_af_8  in_af;  
label define in_af   
	0           "NOT APPLICABLE IF UNDER 15,"   
	1           "YES"                           
	2           "NO"                            
;
label values usrve_1  usrve;  
label values usrve_2  usrve;  
label values usrve_3  usrve;  
label values usrve_4  usrve;  
label values usrve_5  usrve;  
label values usrve_6  usrve;  
label values usrve_7  usrve;  
label values usrve_8  usrve;  
label define usrve   
	0           "NOT APPLICABLE, NOT IN"        
	1           "VIETNAM ERA, AUG'64-APR'75"    
	2           "KOREAN CONFLICT"               
	3           "WORLD WAR II, SEPT'40-JULY'47" 
	4           "WORLD WAR I, APR'17-NOV'18"    
	5           "MAY 1975 OR LATER"             
	6           "OTHER SERVICE"                 
	9           "NOT ANSWERED"                  
;
label values u_brthmn u_brthmn;
label define u_brthmn
	-9          "NOT ANSWERED"                  
;
label values u_brthyr u_brthyr;
label define u_brthyr
	-9          "NOT ANSWERED"                  
	1902        "1902 OR EARLIER"               
;
label values u_pngd1  u_pngd; 
label values u_pngd2  u_pngd; 
label values u_pngd3  u_pngd; 
label values u_pngd4  u_pngd; 
label values u_pngd5  u_pngd; 
label values u_pngd6  u_pngd; 
label values u_pngd7  u_pngd; 
label values u_pngd8  u_pngd; 
label define u_pngd  
	-9          "NOT ANSWERED"                  
	0           "NOT IN UNIVERSE, NOT IN"       
	999         "NOT APPLICABLE"                
;
label values entid_g1 entid_g;
label values entid_g2 entid_g;
label values entid_g3 entid_g;
label values entid_g4 entid_g;
label values entid_g5 entid_g;
label values entid_g6 entid_g;
label values entid_g7 entid_g;
label values entid_g8 entid_g;
label define entid_g 
	0           "NOT IN UNIVERSE, NOT IN"       
	99          "NOT APPLICABLE"                
;
label values livqtr01 livqtr; 
label values livqtr02 livqtr; 
label values livqtr03 livqtr; 
label values livqtr04 livqtr; 
label values livqtr05 livqtr; 
label values livqtr06 livqtr; 
label values livqtr07 livqtr; 
label values livqtr08 livqtr; 
label values livqtr09 livqtr; 
label values livqtr10 livqtr; 
label values livqtr11 livqtr; 
label values livqtr12 livqtr; 
label values livqtr13 livqtr; 
label values livqtr14 livqtr; 
label values livqtr15 livqtr; 
label values livqtr16 livqtr; 
label values livqtr17 livqtr; 
label values livqtr18 livqtr; 
label values livqtr19 livqtr; 
label values livqtr20 livqtr; 
label values livqtr21 livqtr; 
label values livqtr22 livqtr; 
label values livqtr23 livqtr; 
label values livqtr24 livqtr; 
label values livqtr25 livqtr; 
label values livqtr26 livqtr; 
label values livqtr27 livqtr; 
label values livqtr28 livqtr; 
label values livqtr29 livqtr; 
label values livqtr30 livqtr; 
label values livqtr31 livqtr; 
label values livqtr32 livqtr; 
label define livqtr  
	0           "NOT APPLICABLE, NOT IN"        
	1           "HOUSE, APARTMENT, FLAT"        
	2           "HU IN NONTRANSIENT HOTEL,"     
	3           "HU, PERMANENT IN TRANSIENT"    
	4           "HU IN ROOMING HOUSE"           
	5           "MOBILE HOME OR TRAILER WITH"   
	6           "MOBILE HOME OR TRAILER WITH"   
	7           "HU NOT SPECIFIED ABOVE"        
	8           "QUARTERS NOT HU IN ROOMING OR" 
	9           "UNIT NOT PERMANENT IN"         
	10          "UNOCCUPIED TENT OR TRAILER"    
	11          "OTHER UNIT NOT SPECIFIED ABOVE"
;
label values tenure01 tenure; 
label values tenure02 tenure; 
label values tenure03 tenure; 
label values tenure04 tenure; 
label values tenure05 tenure; 
label values tenure06 tenure; 
label values tenure07 tenure; 
label values tenure08 tenure; 
label values tenure09 tenure; 
label values tenure10 tenure; 
label values tenure11 tenure; 
label values tenure12 tenure; 
label values tenure13 tenure; 
label values tenure14 tenure; 
label values tenure15 tenure; 
label values tenure16 tenure; 
label values tenure17 tenure; 
label values tenure18 tenure; 
label values tenure19 tenure; 
label values tenure20 tenure; 
label values tenure21 tenure; 
label values tenure22 tenure; 
label values tenure23 tenure; 
label values tenure24 tenure; 
label values tenure25 tenure; 
label values tenure26 tenure; 
label values tenure27 tenure; 
label values tenure28 tenure; 
label values tenure29 tenure; 
label values tenure30 tenure; 
label values tenure31 tenure; 
label values tenure32 tenure; 
label define tenure  
	0           "NOT IN SAMPLE, NONMATCH"       
	1           "OWNED OR BEING BOUGHT BY"      
	2           "RENTED FOR CASH"               
	3           "OCCUPIED WITHOUT PAYMENT OF"   
;
label values pubhou01 pubhou; 
label values pubhou02 pubhou; 
label values pubhou03 pubhou; 
label values pubhou04 pubhou; 
label values pubhou05 pubhou; 
label values pubhou06 pubhou; 
label values pubhou07 pubhou; 
label values pubhou08 pubhou; 
label values pubhou09 pubhou; 
label values pubhou10 pubhou; 
label values pubhou11 pubhou; 
label values pubhou12 pubhou; 
label values pubhou13 pubhou; 
label values pubhou14 pubhou; 
label values pubhou15 pubhou; 
label values pubhou16 pubhou; 
label values pubhou17 pubhou; 
label values pubhou18 pubhou; 
label values pubhou19 pubhou; 
label values pubhou20 pubhou; 
label values pubhou21 pubhou; 
label values pubhou22 pubhou; 
label values pubhou23 pubhou; 
label values pubhou24 pubhou; 
label values pubhou25 pubhou; 
label values pubhou26 pubhou; 
label values pubhou27 pubhou; 
label values pubhou28 pubhou; 
label values pubhou29 pubhou; 
label values pubhou30 pubhou; 
label values pubhou31 pubhou; 
label values pubhou32 pubhou; 
label define pubhou  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values low_re01 low_re; 
label values low_re02 low_re; 
label values low_re03 low_re; 
label values low_re04 low_re; 
label values low_re05 low_re; 
label values low_re06 low_re; 
label values low_re07 low_re; 
label values low_re08 low_re; 
label values low_re09 low_re; 
label values low_re10 low_re; 
label values low_re11 low_re; 
label values low_re12 low_re; 
label values low_re13 low_re; 
label values low_re14 low_re; 
label values low_re15 low_re; 
label values low_re16 low_re; 
label values low_re17 low_re; 
label values low_re18 low_re; 
label values low_re19 low_re; 
label values low_re20 low_re; 
label values low_re21 low_re; 
label values low_re22 low_re; 
label values low_re23 low_re; 
label values low_re24 low_re; 
label values low_re25 low_re; 
label values low_re26 low_re; 
label values low_re27 low_re; 
label values low_re28 low_re; 
label values low_re29 low_re; 
label values low_re30 low_re; 
label values low_re31 low_re; 
label values low_re32 low_re; 
label define low_re  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values engry_y1 engry_y;
label values engry_y2 engry_y;
label values engry_y3 engry_y;
label values engry_y4 engry_y;
label values engry_y5 engry_y;
label values engry_y6 engry_y;
label values engry_y7 engry_y;
label values engry_y8 engry_y;
label define engry_y 
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "YES"                           
	2           "NO"                            
;
label values engryty1 engryty;
label values engryty2 engryty;
label values engryty3 engryty;
label values engryty4 engryty;
label values engryty5 engryty;
label values engryty6 engryty;
label values engryty7 engryty;
label values engryty8 engryty;
label define engryty 
	0           "NOT APPLICABLE, NOT IN SAMPLE" 
	1           "CHECKS SENT TO HOUSEHOLD"      
	2           "COUPONS OR VOUCHERS SENT TO"   
	3           "PAYMENTS SENT ELSEWHERE"       
	4           "CHECKS AND COUPONS OR"         
	5           "CHECKS SENT TO HOUSEHOLD AND"  
	6           "COUPONS OR VOUCHER SENT TO"    
	7           "ALL THREE TYPES OF ASSISTANCE" 
;
label values engryam1 engryam;
label values engryam2 engryam;
label values engryam3 engryam;
label values engryam4 engryam;
label values engryam5 engryam;
label values engryam6 engryam;
label values engryam7 engryam;
label values engryam8 engryam;
label define engryam 
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values typelun1 typelun;
label values typelun2 typelun;
label values typelun3 typelun;
label values typelun4 typelun;
label values typelun5 typelun;
label values typelun6 typelun;
label values typelun7 typelun;
label values typelun8 typelun;
label define typelun 
	0           "NOT APPLICABLE, NOT IN SAMPLE" 
	1           "FREE"                          
	2           "REDUCED-PRICE"                 
	3           "BOTH"                          
;
label values num_lun1 num_lun;
label values num_lun2 num_lun;
label values num_lun3 num_lun;
label values num_lun4 num_lun;
label values num_lun5 num_lun;
label values num_lun6 num_lun;
label values num_lun7 num_lun;
label values num_lun8 num_lun;
label define num_lun 
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values typebrk1 typebrk;
label values typebrk2 typebrk;
label values typebrk3 typebrk;
label values typebrk4 typebrk;
label values typebrk5 typebrk;
label values typebrk6 typebrk;
label values typebrk7 typebrk;
label values typebrk8 typebrk;
label define typebrk 
	0           "NOT APPLICABLE, NOT IN SAMPLE" 
	1           "FREE"                          
	2           "REDUCED-PRICE"                 
	3           "BOTH"                          
;
label values num_brk1 num_brk;
label values num_brk2 num_brk;
label values num_brk3 num_brk;
label values num_brk4 num_brk;
label values num_brk5 num_brk;
label values num_brk6 num_brk;
label values num_brk7 num_brk;
label values num_brk8 num_brk;
label define num_brk 
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values pubrntyn pubrntyn;
label define pubrntyn
	0           "NOT IN SAMPLE IN WAVE 1"       
	1           "YES"                           
	2           "NO"                            
;
label values pubrnamt pubrnamt;
label define pubrnamt
	0           "NOT APPLICABLE"                
;
label values utlpayyn utlpayyn;
label define utlpayyn
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values fullrent fullrent;
label define fullrent
	0           "NOT APPLICABLE"                
;
label values state_1  state;  
label values state_2  state;  
label values state_3  state;  
label values state_4  state;  
label values state_5  state;  
label values state_6  state;  
label values state_7  state;  
label values state_8  state;  
label define state   
	0           "NONMATCH"                      
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
	61          "MAINE, VERMONT"                
	62          "IOWA, NORTH DAKOTA, SOUTH"     
	63          "ALASKA, IDAHO, MONTANA,"       
;
label values sc1332   sc1332l;
label define sc1332l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "LESS THAN 6 MONTHS"            
	2           "6 TO 23 MONTHS"                
	3           "2 TO 19 YEARS"                 
	4           "20 OR MORE YEARS"              
;
label values sc1334   sc1334l;
label define sc1334l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "YES"                           
	2           "NO"                            
;
label values sc1336   sc1336l;
label define sc1336l 
	-2          "REFUSED"                       
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "1-10%"                         
	2           "11-29%"                        
	3           "30-49%"                        
	4           "50%"                           
	5           "51-89%"                        
	6           "90-99%"                        
	7           "100%"                          
	101         "NO RATING"                     
;
label values sc1346   sc1346l;
label define sc1346l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "RETIRED"                       
	2           "DISABLED"                      
	3           "WIDOW,ED OR SURVIVING CHILD"   
	4           "SPOUSE OR DEPENDENT CHILD"     
	5           "SOME OTHER REASON"             
;
label values sc1348   sc1348l;
label define sc1348l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "RETIRED"                       
	2           "DISABLED"                      
	3           "WIDOW,ED OR SURVIVING CHILD"   
	4           "SPOUSE OR DEPENDENT CHILD"     
	5           "NO OTHER REASON"               
;
label values sc1360   sc1360l;
label define sc1360l 
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "YES"                           
	2           "NO"                            
;
label values sc1418   sc1418l;
label define sc1418l 
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "WIDOWED"                       
	2           "DIVORCED"                      
	3           "BOTH WIDOWED AND DIVORCED"     
	4           "NO"                            
;
label values sc1456   sc1456l;
label define sc1456l 
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "YES, IN THE SERVICE"           
	2           "YES, FROM SERVICE-RELATED"     
	3           "NO"                            
;
label values sc1468   sc1468l;
label define sc1468l 
	0           "NOT IN UNIVERSE OR"            
	1           "HOSPITAL ONLY, TYPE A"         
	2           "MEDICAL ONLY, TYPE B"          
	3           "BOTH HOSPITAL AND MEDICAL"     
	4           "CARD NOT AVAILABLE"            
;
label values sc1472   sc1472l;
label define sc1472l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO"                            
;
label values disab    disab;  
label define disab   
	0           "NOT IN UNIVERSE, UNDER 15"     
	1           "EVER DISABLED MARKED ON THE"   
;
label values att_sch1 att_sch;
label values att_sch2 att_sch;
label values att_sch3 att_sch;
label values att_sch4 att_sch;
label values att_sch5 att_sch;
label values att_sch6 att_sch;
label values att_sch7 att_sch;
label values att_sch8 att_sch;
label define att_sch 
	0           "NOT IN UNIVERSE"               
	1           "YES, FULL-TIME"                
	2           "YES, PART-TIME"                
	3           "NO - SKIP TO SC1694"           
;
label values enrl_m01 enrl_m; 
label values enrl_m02 enrl_m; 
label values enrl_m03 enrl_m; 
label values enrl_m04 enrl_m; 
label values enrl_m05 enrl_m; 
label values enrl_m06 enrl_m; 
label values enrl_m07 enrl_m; 
label values enrl_m08 enrl_m; 
label values enrl_m09 enrl_m; 
label values enrl_m10 enrl_m; 
label values enrl_m11 enrl_m; 
label values enrl_m12 enrl_m; 
label values enrl_m13 enrl_m; 
label values enrl_m14 enrl_m; 
label values enrl_m15 enrl_m; 
label values enrl_m16 enrl_m; 
label values enrl_m17 enrl_m; 
label values enrl_m18 enrl_m; 
label values enrl_m19 enrl_m; 
label values enrl_m20 enrl_m; 
label values enrl_m21 enrl_m; 
label values enrl_m22 enrl_m; 
label values enrl_m23 enrl_m; 
label values enrl_m24 enrl_m; 
label values enrl_m25 enrl_m; 
label values enrl_m26 enrl_m; 
label values enrl_m27 enrl_m; 
label values enrl_m28 enrl_m; 
label values enrl_m29 enrl_m; 
label values enrl_m30 enrl_m; 
label values enrl_m31 enrl_m; 
label values enrl_m32 enrl_m; 
label define enrl_m  
	0           "NOT ENROLLED, NOT IN"          
	1           "ENROLLED DURING THAT MONTH"    
;
label values ed_leve1 ed_leve;
label values ed_leve2 ed_leve;
label values ed_leve3 ed_leve;
label values ed_leve4 ed_leve;
label values ed_leve5 ed_leve;
label values ed_leve6 ed_leve;
label values ed_leve7 ed_leve;
label values ed_leve8 ed_leve;
label define ed_leve 
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "ELEMENTARY GRADES 1-8"         
	2           "HIGH SCHOOL GRADES 9-12"       
	3           "COLLEGE YEAR 1"                
	4           "COLLEGE YEAR 2"                
	5           "COLLEGE YEAR 3"                
	6           "COLLEGE YEAR 4"                
	7           "COLLEGE YEAR 5"                
	8           "COLLEGE YEAR 6"                
	9           "VOCATIONAL SCHOOL"             
	10          "TECHNICAL SCHOOL"              
	11          "BUSINESS SCHOOL"               
;
label values ed_fina1 ed_fina;
label values ed_fina2 ed_fina;
label values ed_fina3 ed_fina;
label values ed_fina4 ed_fina;
label values ed_fina5 ed_fina;
label values ed_fina6 ed_fina;
label values ed_fina7 ed_fina;
label values ed_fina8 ed_fina;
label define ed_fina 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO"                            
;
label values sc16721  sc1672l;
label values sc16722  sc1672l;
label values sc16723  sc1672l;
label values sc16724  sc1672l;
label values sc16725  sc1672l;
label values sc16726  sc1672l;
label values sc16727  sc1672l;
label values sc16728  sc1672l;
label define sc1672l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16741  sc1674l;
label values sc16742  sc1674l;
label values sc16743  sc1674l;
label values sc16744  sc1674l;
label values sc16745  sc1674l;
label values sc16746  sc1674l;
label values sc16747  sc1674l;
label values sc16748  sc1674l;
label define sc1674l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16761  sc1676l;
label values sc16762  sc1676l;
label values sc16763  sc1676l;
label values sc16764  sc1676l;
label values sc16765  sc1676l;
label values sc16766  sc1676l;
label values sc16767  sc1676l;
label values sc16768  sc1676l;
label define sc1676l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16781  sc1678l;
label values sc16782  sc1678l;
label values sc16783  sc1678l;
label values sc16784  sc1678l;
label values sc16785  sc1678l;
label values sc16786  sc1678l;
label values sc16787  sc1678l;
label values sc16788  sc1678l;
label define sc1678l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16801  sc1680l;
label values sc16802  sc1680l;
label values sc16803  sc1680l;
label values sc16804  sc1680l;
label values sc16805  sc1680l;
label values sc16806  sc1680l;
label values sc16807  sc1680l;
label values sc16808  sc1680l;
label define sc1680l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16821  sc1682l;
label values sc16822  sc1682l;
label values sc16823  sc1682l;
label values sc16824  sc1682l;
label values sc16825  sc1682l;
label values sc16826  sc1682l;
label values sc16827  sc1682l;
label values sc16828  sc1682l;
label define sc1682l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16841  sc1684l;
label values sc16842  sc1684l;
label values sc16843  sc1684l;
label values sc16844  sc1684l;
label values sc16845  sc1684l;
label values sc16846  sc1684l;
label values sc16847  sc1684l;
label values sc16848  sc1684l;
label define sc1684l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16861  sc1686l;
label values sc16862  sc1686l;
label values sc16863  sc1686l;
label values sc16864  sc1686l;
label values sc16865  sc1686l;
label values sc16866  sc1686l;
label values sc16867  sc1686l;
label values sc16868  sc1686l;
label define sc1686l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16881  sc1688l;
label values sc16882  sc1688l;
label values sc16883  sc1688l;
label values sc16884  sc1688l;
label values sc16885  sc1688l;
label values sc16886  sc1688l;
label values sc16887  sc1688l;
label values sc16888  sc1688l;
label define sc1688l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16901  sc1690l;
label values sc16902  sc1690l;
label values sc16903  sc1690l;
label values sc16904  sc1690l;
label values sc16905  sc1690l;
label values sc16906  sc1690l;
label values sc16907  sc1690l;
label values sc16908  sc1690l;
label define sc1690l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16921  sc1692l;
label values sc16922  sc1692l;
label values sc16923  sc1692l;
label values sc16924  sc1692l;
label values sc16925  sc1692l;
label values sc16926  sc1692l;
label values sc16927  sc1692l;
label values sc16928  sc1692l;
label define sc1692l 
	0           "NOT MARKED AS A KIND OF"       
	1           "MARKED AS A KIND OF"           
;
label values sc16961  sc1696l;
label values sc16962  sc1696l;
label values sc16963  sc1696l;
label values sc16964  sc1696l;
label values sc16965  sc1696l;
label values sc16966  sc1696l;
label values sc16967  sc1696l;
label values sc16968  sc1696l;
label define sc1696l 
	0           "NOT IN UNIVERSE"               
	1           "YES"                           
	2           "NO"                            
;
label values esr_01   esr;    
label values esr_02   esr;    
label values esr_03   esr;    
label values esr_04   esr;    
label values esr_05   esr;    
label values esr_06   esr;    
label values esr_07   esr;    
label values esr_08   esr;    
label values esr_09   esr;    
label values esr_10   esr;    
label values esr_11   esr;    
label values esr_12   esr;    
label values esr_13   esr;    
label values esr_14   esr;    
label values esr_15   esr;    
label values esr_16   esr;    
label values esr_17   esr;    
label values esr_18   esr;    
label values esr_19   esr;    
label values esr_20   esr;    
label values esr_21   esr;    
label values esr_22   esr;    
label values esr_23   esr;    
label values esr_24   esr;    
label values esr_25   esr;    
label values esr_26   esr;    
label values esr_27   esr;    
label values esr_28   esr;    
label values esr_29   esr;    
label values esr_30   esr;    
label values esr_31   esr;    
label values esr_32   esr;    
label define esr     
	0           "NOT APPLICABLE, NOT IN"        
	1           "WITH A JOB ENTIRE MONTH,"      
	2           "WITH A JOB ENTIRE MONTH,"      
	3           "WITH A JOB ENTIRE MONTH,"      
	4           "WITH JOB ONE OR MORE WEEKS,"   
	5           "WITH JOB ONE OR MORE WEEKS,"   
	6           "NO JOB DURING MONTH, SPENT"    
	7           "NO JOB DURING MONTH, SPENT"    
	8           "NO JOB DURING MONTH, NO TIME"  
;
label values wksper01 wksper; 
label values wksper02 wksper; 
label values wksper03 wksper; 
label values wksper04 wksper; 
label values wksper05 wksper; 
label values wksper06 wksper; 
label values wksper07 wksper; 
label values wksper08 wksper; 
label values wksper09 wksper; 
label values wksper10 wksper; 
label values wksper11 wksper; 
label values wksper12 wksper; 
label values wksper13 wksper; 
label values wksper14 wksper; 
label values wksper15 wksper; 
label values wksper16 wksper; 
label values wksper17 wksper; 
label values wksper18 wksper; 
label values wksper19 wksper; 
label values wksper20 wksper; 
label values wksper21 wksper; 
label values wksper22 wksper; 
label values wksper23 wksper; 
label values wksper24 wksper; 
label values wksper25 wksper; 
label values wksper26 wksper; 
label values wksper27 wksper; 
label values wksper28 wksper; 
label values wksper29 wksper; 
label values wksper30 wksper; 
label values wksper31 wksper; 
label values wksper32 wksper; 
label define wksper  
	0           "NOT APPLICABLE, NOT IN"        
	4           "FOUR WEEKS"                    
	5           "FIVE WEEKS"                    
;
label values mthjbw01 mthjbw; 
label values mthjbw02 mthjbw; 
label values mthjbw03 mthjbw; 
label values mthjbw04 mthjbw; 
label values mthjbw05 mthjbw; 
label values mthjbw06 mthjbw; 
label values mthjbw07 mthjbw; 
label values mthjbw08 mthjbw; 
label values mthjbw09 mthjbw; 
label values mthjbw10 mthjbw; 
label values mthjbw11 mthjbw; 
label values mthjbw12 mthjbw; 
label values mthjbw13 mthjbw; 
label values mthjbw14 mthjbw; 
label values mthjbw15 mthjbw; 
label values mthjbw16 mthjbw; 
label values mthjbw17 mthjbw; 
label values mthjbw18 mthjbw; 
label values mthjbw19 mthjbw; 
label values mthjbw20 mthjbw; 
label values mthjbw21 mthjbw; 
label values mthjbw22 mthjbw; 
label values mthjbw23 mthjbw; 
label values mthjbw24 mthjbw; 
label values mthjbw25 mthjbw; 
label values mthjbw26 mthjbw; 
label values mthjbw27 mthjbw; 
label values mthjbw28 mthjbw; 
label values mthjbw29 mthjbw; 
label values mthjbw30 mthjbw; 
label values mthjbw31 mthjbw; 
label values mthjbw32 mthjbw; 
label define mthjbw  
	0           "0 WEEKS OR NOT APPLICABLE,"    
	1           "1 WEEKS"                       
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS, ONLY APPLICABLE FOR"  
;
label values mthwop01 mthwop; 
label values mthwop02 mthwop; 
label values mthwop03 mthwop; 
label values mthwop04 mthwop; 
label values mthwop05 mthwop; 
label values mthwop06 mthwop; 
label values mthwop07 mthwop; 
label values mthwop08 mthwop; 
label values mthwop09 mthwop; 
label values mthwop10 mthwop; 
label values mthwop11 mthwop; 
label values mthwop12 mthwop; 
label values mthwop13 mthwop; 
label values mthwop14 mthwop; 
label values mthwop15 mthwop; 
label values mthwop16 mthwop; 
label values mthwop17 mthwop; 
label values mthwop18 mthwop; 
label values mthwop19 mthwop; 
label values mthwop20 mthwop; 
label values mthwop21 mthwop; 
label values mthwop22 mthwop; 
label values mthwop23 mthwop; 
label values mthwop24 mthwop; 
label values mthwop25 mthwop; 
label values mthwop26 mthwop; 
label values mthwop27 mthwop; 
label values mthwop28 mthwop; 
label values mthwop29 mthwop; 
label values mthwop30 mthwop; 
label values mthwop31 mthwop; 
label values mthwop32 mthwop; 
label define mthwop  
	0           "0 WEEKS OR NOT APPLICABLE,"    
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS, ONLY APPLICABLE FOR"  
;
label values mthwks01 mthwks; 
label values mthwks02 mthwks; 
label values mthwks03 mthwks; 
label values mthwks04 mthwks; 
label values mthwks05 mthwks; 
label values mthwks06 mthwks; 
label values mthwks07 mthwks; 
label values mthwks08 mthwks; 
label values mthwks09 mthwks; 
label values mthwks10 mthwks; 
label values mthwks11 mthwks; 
label values mthwks12 mthwks; 
label values mthwks13 mthwks; 
label values mthwks14 mthwks; 
label values mthwks15 mthwks; 
label values mthwks16 mthwks; 
label values mthwks17 mthwks; 
label values mthwks18 mthwks; 
label values mthwks19 mthwks; 
label values mthwks20 mthwks; 
label values mthwks21 mthwks; 
label values mthwks22 mthwks; 
label values mthwks23 mthwks; 
label values mthwks24 mthwks; 
label values mthwks25 mthwks; 
label values mthwks26 mthwks; 
label values mthwks27 mthwks; 
label values mthwks28 mthwks; 
label values mthwks29 mthwks; 
label values mthwks30 mthwks; 
label values mthwks31 mthwks; 
label values mthwks32 mthwks; 
label define mthwks  
	0           "NONE OR NOT APPLICABLE, NOT"   
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS, ONLY APPLICABLE FOR"  
;
label values usualhr1 usualhr;
label values usualhr2 usualhr;
label values usualhr3 usualhr;
label values usualhr4 usualhr;
label values usualhr5 usualhr;
label values usualhr6 usualhr;
label values usualhr7 usualhr;
label values usualhr8 usualhr;
label define usualhr 
	0           "NOT IN UNIVERSE"               
;
label values ws1_ei01 ws1_ei; 
label values ws1_ei02 ws1_ei; 
label values ws1_ei03 ws1_ei; 
label values ws1_ei04 ws1_ei; 
label values ws1_ei05 ws1_ei; 
label values ws1_ei06 ws1_ei; 
label values ws1_ei07 ws1_ei; 
label values ws1_ei08 ws1_ei; 
label values ws1_ei09 ws1_ei; 
label values ws1_ei10 ws1_ei; 
label values ws1_ei11 ws1_ei; 
label values ws1_ei12 ws1_ei; 
label values ws1_ei13 ws1_ei; 
label values ws1_ei14 ws1_ei; 
label values ws1_ei15 ws1_ei; 
label values ws1_ei16 ws1_ei; 
label values ws1_ei17 ws1_ei; 
label values ws1_ei18 ws1_ei; 
label values ws1_ei19 ws1_ei; 
label values ws1_ei20 ws1_ei; 
label values ws1_ei21 ws1_ei; 
label values ws1_ei22 ws1_ei; 
label values ws1_ei23 ws1_ei; 
label values ws1_ei24 ws1_ei; 
label values ws1_ei25 ws1_ei; 
label values ws1_ei26 ws1_ei; 
label values ws1_ei27 ws1_ei; 
label values ws1_ei28 ws1_ei; 
label values ws1_ei29 ws1_ei; 
label values ws1_ei30 ws1_ei; 
label values ws1_ei31 ws1_ei; 
label values ws1_ei32 ws1_ei; 
label define ws1_ei  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws2_ei01 ws2_ei; 
label values ws2_ei02 ws2_ei; 
label values ws2_ei03 ws2_ei; 
label values ws2_ei04 ws2_ei; 
label values ws2_ei05 ws2_ei; 
label values ws2_ei06 ws2_ei; 
label values ws2_ei07 ws2_ei; 
label values ws2_ei08 ws2_ei; 
label values ws2_ei09 ws2_ei; 
label values ws2_ei10 ws2_ei; 
label values ws2_ei11 ws2_ei; 
label values ws2_ei12 ws2_ei; 
label values ws2_ei13 ws2_ei; 
label values ws2_ei14 ws2_ei; 
label values ws2_ei15 ws2_ei; 
label values ws2_ei16 ws2_ei; 
label values ws2_ei17 ws2_ei; 
label values ws2_ei18 ws2_ei; 
label values ws2_ei19 ws2_ei; 
label values ws2_ei20 ws2_ei; 
label values ws2_ei21 ws2_ei; 
label values ws2_ei22 ws2_ei; 
label values ws2_ei23 ws2_ei; 
label values ws2_ei24 ws2_ei; 
label values ws2_ei25 ws2_ei; 
label values ws2_ei26 ws2_ei; 
label values ws2_ei27 ws2_ei; 
label values ws2_ei28 ws2_ei; 
label values ws2_ei29 ws2_ei; 
label values ws2_ei30 ws2_ei; 
label values ws2_ei31 ws2_ei; 
label values ws2_ei32 ws2_ei; 
label define ws2_ei  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws1_cl01 ws1_cl; 
label values ws1_cl02 ws1_cl; 
label values ws1_cl03 ws1_cl; 
label values ws1_cl04 ws1_cl; 
label values ws1_cl05 ws1_cl; 
label values ws1_cl06 ws1_cl; 
label values ws1_cl07 ws1_cl; 
label values ws1_cl08 ws1_cl; 
label values ws1_cl09 ws1_cl; 
label values ws1_cl10 ws1_cl; 
label values ws1_cl11 ws1_cl; 
label values ws1_cl12 ws1_cl; 
label values ws1_cl13 ws1_cl; 
label values ws1_cl14 ws1_cl; 
label values ws1_cl15 ws1_cl; 
label values ws1_cl16 ws1_cl; 
label values ws1_cl17 ws1_cl; 
label values ws1_cl18 ws1_cl; 
label values ws1_cl19 ws1_cl; 
label values ws1_cl20 ws1_cl; 
label values ws1_cl21 ws1_cl; 
label values ws1_cl22 ws1_cl; 
label values ws1_cl23 ws1_cl; 
label values ws1_cl24 ws1_cl; 
label values ws1_cl25 ws1_cl; 
label values ws1_cl26 ws1_cl; 
label values ws1_cl27 ws1_cl; 
label values ws1_cl28 ws1_cl; 
label values ws1_cl29 ws1_cl; 
label values ws1_cl30 ws1_cl; 
label values ws1_cl31 ws1_cl; 
label values ws1_cl32 ws1_cl; 
label define ws1_cl  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "A PRIVATE COMPANY OR"          
	2           "FEDERAL GOVERNMENT, EXCLUDE"   
	3           "STATE GOVERNMENT"              
	4           "LOCAL GOVERNMENT"              
	5           "ARMED FORCES"                  
	6           "UNPAID IN FAMILY BUSINESS OR"  
;
label values ws2_cl01 ws2_cl; 
label values ws2_cl02 ws2_cl; 
label values ws2_cl03 ws2_cl; 
label values ws2_cl04 ws2_cl; 
label values ws2_cl05 ws2_cl; 
label values ws2_cl06 ws2_cl; 
label values ws2_cl07 ws2_cl; 
label values ws2_cl08 ws2_cl; 
label values ws2_cl09 ws2_cl; 
label values ws2_cl10 ws2_cl; 
label values ws2_cl11 ws2_cl; 
label values ws2_cl12 ws2_cl; 
label values ws2_cl13 ws2_cl; 
label values ws2_cl14 ws2_cl; 
label values ws2_cl15 ws2_cl; 
label values ws2_cl16 ws2_cl; 
label values ws2_cl17 ws2_cl; 
label values ws2_cl18 ws2_cl; 
label values ws2_cl19 ws2_cl; 
label values ws2_cl20 ws2_cl; 
label values ws2_cl21 ws2_cl; 
label values ws2_cl22 ws2_cl; 
label values ws2_cl23 ws2_cl; 
label values ws2_cl24 ws2_cl; 
label values ws2_cl25 ws2_cl; 
label values ws2_cl26 ws2_cl; 
label values ws2_cl27 ws2_cl; 
label values ws2_cl28 ws2_cl; 
label values ws2_cl29 ws2_cl; 
label values ws2_cl30 ws2_cl; 
label values ws2_cl31 ws2_cl; 
label values ws2_cl32 ws2_cl; 
label define ws2_cl  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "A PRIVATE COMPANY OR"          
	2           "FEDERAL GOVERNMENT, EXCLUDE"   
	3           "STATE GOVERNMENT"              
	4           "LOCAL GOVERNMENT"              
	5           "ARMED FORCES"                  
	6           "UNPAID IN FAMILY BUSINESS OR"  
;
label values ws1_wk01 ws1_wk; 
label values ws1_wk02 ws1_wk; 
label values ws1_wk03 ws1_wk; 
label values ws1_wk04 ws1_wk; 
label values ws1_wk05 ws1_wk; 
label values ws1_wk06 ws1_wk; 
label values ws1_wk07 ws1_wk; 
label values ws1_wk08 ws1_wk; 
label values ws1_wk09 ws1_wk; 
label values ws1_wk10 ws1_wk; 
label values ws1_wk11 ws1_wk; 
label values ws1_wk12 ws1_wk; 
label values ws1_wk13 ws1_wk; 
label values ws1_wk14 ws1_wk; 
label values ws1_wk15 ws1_wk; 
label values ws1_wk16 ws1_wk; 
label values ws1_wk17 ws1_wk; 
label values ws1_wk18 ws1_wk; 
label values ws1_wk19 ws1_wk; 
label values ws1_wk20 ws1_wk; 
label values ws1_wk21 ws1_wk; 
label values ws1_wk22 ws1_wk; 
label values ws1_wk23 ws1_wk; 
label values ws1_wk24 ws1_wk; 
label values ws1_wk25 ws1_wk; 
label values ws1_wk26 ws1_wk; 
label values ws1_wk27 ws1_wk; 
label values ws1_wk28 ws1_wk; 
label values ws1_wk29 ws1_wk; 
label values ws1_wk30 ws1_wk; 
label values ws1_wk31 ws1_wk; 
label values ws1_wk32 ws1_wk; 
label define ws1_wk  
	0           "NONE OR NOT IN UNIVERSE IF"    
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS"                       
;
label values ws2_wk01 ws2_wk; 
label values ws2_wk02 ws2_wk; 
label values ws2_wk03 ws2_wk; 
label values ws2_wk04 ws2_wk; 
label values ws2_wk05 ws2_wk; 
label values ws2_wk06 ws2_wk; 
label values ws2_wk07 ws2_wk; 
label values ws2_wk08 ws2_wk; 
label values ws2_wk09 ws2_wk; 
label values ws2_wk10 ws2_wk; 
label values ws2_wk11 ws2_wk; 
label values ws2_wk12 ws2_wk; 
label values ws2_wk13 ws2_wk; 
label values ws2_wk14 ws2_wk; 
label values ws2_wk15 ws2_wk; 
label values ws2_wk16 ws2_wk; 
label values ws2_wk17 ws2_wk; 
label values ws2_wk18 ws2_wk; 
label values ws2_wk19 ws2_wk; 
label values ws2_wk20 ws2_wk; 
label values ws2_wk21 ws2_wk; 
label values ws2_wk22 ws2_wk; 
label values ws2_wk23 ws2_wk; 
label values ws2_wk24 ws2_wk; 
label values ws2_wk25 ws2_wk; 
label values ws2_wk26 ws2_wk; 
label values ws2_wk27 ws2_wk; 
label values ws2_wk28 ws2_wk; 
label values ws2_wk29 ws2_wk; 
label values ws2_wk30 ws2_wk; 
label values ws2_wk31 ws2_wk; 
label values ws2_wk32 ws2_wk; 
label define ws2_wk  
	0           "NONE OR NOT IN UNIVERSE IF"    
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS"                       
;
label values ws1_hr01 ws1_hr; 
label values ws1_hr02 ws1_hr; 
label values ws1_hr03 ws1_hr; 
label values ws1_hr04 ws1_hr; 
label values ws1_hr05 ws1_hr; 
label values ws1_hr06 ws1_hr; 
label values ws1_hr07 ws1_hr; 
label values ws1_hr08 ws1_hr; 
label values ws1_hr09 ws1_hr; 
label values ws1_hr10 ws1_hr; 
label values ws1_hr11 ws1_hr; 
label values ws1_hr12 ws1_hr; 
label values ws1_hr13 ws1_hr; 
label values ws1_hr14 ws1_hr; 
label values ws1_hr15 ws1_hr; 
label values ws1_hr16 ws1_hr; 
label values ws1_hr17 ws1_hr; 
label values ws1_hr18 ws1_hr; 
label values ws1_hr19 ws1_hr; 
label values ws1_hr20 ws1_hr; 
label values ws1_hr21 ws1_hr; 
label values ws1_hr22 ws1_hr; 
label values ws1_hr23 ws1_hr; 
label values ws1_hr24 ws1_hr; 
label values ws1_hr25 ws1_hr; 
label values ws1_hr26 ws1_hr; 
label values ws1_hr27 ws1_hr; 
label values ws1_hr28 ws1_hr; 
label values ws1_hr29 ws1_hr; 
label values ws1_hr30 ws1_hr; 
label values ws1_hr31 ws1_hr; 
label values ws1_hr32 ws1_hr; 
label define ws1_hr  
	-3          "NONE"                          
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws2_hr01 ws2_hr; 
label values ws2_hr02 ws2_hr; 
label values ws2_hr03 ws2_hr; 
label values ws2_hr04 ws2_hr; 
label values ws2_hr05 ws2_hr; 
label values ws2_hr06 ws2_hr; 
label values ws2_hr07 ws2_hr; 
label values ws2_hr08 ws2_hr; 
label values ws2_hr09 ws2_hr; 
label values ws2_hr10 ws2_hr; 
label values ws2_hr11 ws2_hr; 
label values ws2_hr12 ws2_hr; 
label values ws2_hr13 ws2_hr; 
label values ws2_hr14 ws2_hr; 
label values ws2_hr15 ws2_hr; 
label values ws2_hr16 ws2_hr; 
label values ws2_hr17 ws2_hr; 
label values ws2_hr18 ws2_hr; 
label values ws2_hr19 ws2_hr; 
label values ws2_hr20 ws2_hr; 
label values ws2_hr21 ws2_hr; 
label values ws2_hr22 ws2_hr; 
label values ws2_hr23 ws2_hr; 
label values ws2_hr24 ws2_hr; 
label values ws2_hr25 ws2_hr; 
label values ws2_hr26 ws2_hr; 
label values ws2_hr27 ws2_hr; 
label values ws2_hr28 ws2_hr; 
label values ws2_hr29 ws2_hr; 
label values ws2_hr30 ws2_hr; 
label values ws2_hr31 ws2_hr; 
label values ws2_hr32 ws2_hr; 
label define ws2_hr  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws1_ra01 ws1_ra; 
label values ws1_ra02 ws1_ra; 
label values ws1_ra03 ws1_ra; 
label values ws1_ra04 ws1_ra; 
label values ws1_ra05 ws1_ra; 
label values ws1_ra06 ws1_ra; 
label values ws1_ra07 ws1_ra; 
label values ws1_ra08 ws1_ra; 
label values ws1_ra09 ws1_ra; 
label values ws1_ra10 ws1_ra; 
label values ws1_ra11 ws1_ra; 
label values ws1_ra12 ws1_ra; 
label values ws1_ra13 ws1_ra; 
label values ws1_ra14 ws1_ra; 
label values ws1_ra15 ws1_ra; 
label values ws1_ra16 ws1_ra; 
label values ws1_ra17 ws1_ra; 
label values ws1_ra18 ws1_ra; 
label values ws1_ra19 ws1_ra; 
label values ws1_ra20 ws1_ra; 
label values ws1_ra21 ws1_ra; 
label values ws1_ra22 ws1_ra; 
label values ws1_ra23 ws1_ra; 
label values ws1_ra24 ws1_ra; 
label values ws1_ra25 ws1_ra; 
label values ws1_ra26 ws1_ra; 
label values ws1_ra27 ws1_ra; 
label values ws1_ra28 ws1_ra; 
label values ws1_ra29 ws1_ra; 
label values ws1_ra30 ws1_ra; 
label values ws1_ra31 ws1_ra; 
label values ws1_ra32 ws1_ra; 
label define ws1_ra  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws2_ra01 ws2_ra; 
label values ws2_ra02 ws2_ra; 
label values ws2_ra03 ws2_ra; 
label values ws2_ra04 ws2_ra; 
label values ws2_ra05 ws2_ra; 
label values ws2_ra06 ws2_ra; 
label values ws2_ra07 ws2_ra; 
label values ws2_ra08 ws2_ra; 
label values ws2_ra09 ws2_ra; 
label values ws2_ra10 ws2_ra; 
label values ws2_ra11 ws2_ra; 
label values ws2_ra12 ws2_ra; 
label values ws2_ra13 ws2_ra; 
label values ws2_ra14 ws2_ra; 
label values ws2_ra15 ws2_ra; 
label values ws2_ra16 ws2_ra; 
label values ws2_ra17 ws2_ra; 
label values ws2_ra18 ws2_ra; 
label values ws2_ra19 ws2_ra; 
label values ws2_ra20 ws2_ra; 
label values ws2_ra21 ws2_ra; 
label values ws2_ra22 ws2_ra; 
label values ws2_ra23 ws2_ra; 
label values ws2_ra24 ws2_ra; 
label values ws2_ra25 ws2_ra; 
label values ws2_ra26 ws2_ra; 
label values ws2_ra27 ws2_ra; 
label values ws2_ra28 ws2_ra; 
label values ws2_ra29 ws2_ra; 
label values ws2_ra30 ws2_ra; 
label values ws2_ra31 ws2_ra; 
label values ws2_ra32 ws2_ra; 
label define ws2_ra  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values se1_bi01 se1_bi; 
label values se1_bi02 se1_bi; 
label values se1_bi03 se1_bi; 
label values se1_bi04 se1_bi; 
label values se1_bi05 se1_bi; 
label values se1_bi06 se1_bi; 
label values se1_bi07 se1_bi; 
label values se1_bi08 se1_bi; 
label values se1_bi09 se1_bi; 
label values se1_bi10 se1_bi; 
label values se1_bi11 se1_bi; 
label values se1_bi12 se1_bi; 
label values se1_bi13 se1_bi; 
label values se1_bi14 se1_bi; 
label values se1_bi15 se1_bi; 
label values se1_bi16 se1_bi; 
label values se1_bi17 se1_bi; 
label values se1_bi18 se1_bi; 
label values se1_bi19 se1_bi; 
label values se1_bi20 se1_bi; 
label values se1_bi21 se1_bi; 
label values se1_bi22 se1_bi; 
label values se1_bi23 se1_bi; 
label values se1_bi24 se1_bi; 
label values se1_bi25 se1_bi; 
label values se1_bi26 se1_bi; 
label values se1_bi27 se1_bi; 
label values se1_bi28 se1_bi; 
label values se1_bi29 se1_bi; 
label values se1_bi30 se1_bi; 
label values se1_bi31 se1_bi; 
label values se1_bi32 se1_bi; 
label define se1_bi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values se2_bi01 se2_bi; 
label values se2_bi02 se2_bi; 
label values se2_bi03 se2_bi; 
label values se2_bi04 se2_bi; 
label values se2_bi05 se2_bi; 
label values se2_bi06 se2_bi; 
label values se2_bi07 se2_bi; 
label values se2_bi08 se2_bi; 
label values se2_bi09 se2_bi; 
label values se2_bi10 se2_bi; 
label values se2_bi11 se2_bi; 
label values se2_bi12 se2_bi; 
label values se2_bi13 se2_bi; 
label values se2_bi14 se2_bi; 
label values se2_bi15 se2_bi; 
label values se2_bi16 se2_bi; 
label values se2_bi17 se2_bi; 
label values se2_bi18 se2_bi; 
label values se2_bi19 se2_bi; 
label values se2_bi20 se2_bi; 
label values se2_bi21 se2_bi; 
label values se2_bi22 se2_bi; 
label values se2_bi23 se2_bi; 
label values se2_bi24 se2_bi; 
label values se2_bi25 se2_bi; 
label values se2_bi26 se2_bi; 
label values se2_bi27 se2_bi; 
label values se2_bi28 se2_bi; 
label values se2_bi29 se2_bi; 
label values se2_bi30 se2_bi; 
label values se2_bi31 se2_bi; 
label values se2_bi32 se2_bi; 
label define se2_bi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values se1_ty01 se1_ty; 
label values se1_ty02 se1_ty; 
label values se1_ty03 se1_ty; 
label values se1_ty04 se1_ty; 
label values se1_ty05 se1_ty; 
label values se1_ty06 se1_ty; 
label values se1_ty07 se1_ty; 
label values se1_ty08 se1_ty; 
label values se1_ty09 se1_ty; 
label values se1_ty10 se1_ty; 
label values se1_ty11 se1_ty; 
label values se1_ty12 se1_ty; 
label values se1_ty13 se1_ty; 
label values se1_ty14 se1_ty; 
label values se1_ty15 se1_ty; 
label values se1_ty16 se1_ty; 
label values se1_ty17 se1_ty; 
label values se1_ty18 se1_ty; 
label values se1_ty19 se1_ty; 
label values se1_ty20 se1_ty; 
label values se1_ty21 se1_ty; 
label values se1_ty22 se1_ty; 
label values se1_ty23 se1_ty; 
label values se1_ty24 se1_ty; 
label values se1_ty25 se1_ty; 
label values se1_ty26 se1_ty; 
label values se1_ty27 se1_ty; 
label values se1_ty28 se1_ty; 
label values se1_ty29 se1_ty; 
label values se1_ty30 se1_ty; 
label values se1_ty31 se1_ty; 
label values se1_ty32 se1_ty; 
label define se1_ty  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "SOLE PROPRIETORSHIP"           
	2           "PARTNERSHIP"                   
	3           "CORPORATION"                   
;
label values se2_ty01 se2_ty; 
label values se2_ty02 se2_ty; 
label values se2_ty03 se2_ty; 
label values se2_ty04 se2_ty; 
label values se2_ty05 se2_ty; 
label values se2_ty06 se2_ty; 
label values se2_ty07 se2_ty; 
label values se2_ty08 se2_ty; 
label values se2_ty09 se2_ty; 
label values se2_ty10 se2_ty; 
label values se2_ty11 se2_ty; 
label values se2_ty12 se2_ty; 
label values se2_ty13 se2_ty; 
label values se2_ty14 se2_ty; 
label values se2_ty15 se2_ty; 
label values se2_ty16 se2_ty; 
label values se2_ty17 se2_ty; 
label values se2_ty18 se2_ty; 
label values se2_ty19 se2_ty; 
label values se2_ty20 se2_ty; 
label values se2_ty21 se2_ty; 
label values se2_ty22 se2_ty; 
label values se2_ty23 se2_ty; 
label values se2_ty24 se2_ty; 
label values se2_ty25 se2_ty; 
label values se2_ty26 se2_ty; 
label values se2_ty27 se2_ty; 
label values se2_ty28 se2_ty; 
label values se2_ty29 se2_ty; 
label values se2_ty30 se2_ty; 
label values se2_ty31 se2_ty; 
label values se2_ty32 se2_ty; 
label define se2_ty  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "SOLE PROPRIETORSHIP"           
	2           "PARTNERSHIP"                   
	3           "CORPORATION"                   
;
label values se1_in01 se1_in; 
label values se1_in02 se1_in; 
label values se1_in03 se1_in; 
label values se1_in04 se1_in; 
label values se1_in05 se1_in; 
label values se1_in06 se1_in; 
label values se1_in07 se1_in; 
label values se1_in08 se1_in; 
label values se1_in09 se1_in; 
label values se1_in10 se1_in; 
label values se1_in11 se1_in; 
label values se1_in12 se1_in; 
label values se1_in13 se1_in; 
label values se1_in14 se1_in; 
label values se1_in15 se1_in; 
label values se1_in16 se1_in; 
label values se1_in17 se1_in; 
label values se1_in18 se1_in; 
label values se1_in19 se1_in; 
label values se1_in20 se1_in; 
label values se1_in21 se1_in; 
label values se1_in22 se1_in; 
label values se1_in23 se1_in; 
label values se1_in24 se1_in; 
label values se1_in25 se1_in; 
label values se1_in26 se1_in; 
label values se1_in27 se1_in; 
label values se1_in28 se1_in; 
label values se1_in29 se1_in; 
label values se1_in30 se1_in; 
label values se1_in31 se1_in; 
label values se1_in32 se1_in; 
label define se1_in  
	1           "AGRICULTURE, FORESTRY, FISHERIES"
	2           "MINING"                        
	3           "CONSTRUCTION"                  
	4           "MANUFACTURING-NONDURABLE GOODS"
	5           "MANUFACTURING-DURABLE GOODS"   
	6           "TRANSPORTATION, COMM."         
	7           "WHOLESALE TRADE-DURABLE GOODS" 
	8           "WHOLESALE TRADE-NONDURABLE GOODS"
	9           "RETAIL TRADE"                  
	10          "FINANCE, INSURANCE, R.ESTATE"  
	11          "BUSINESS AND REPAIR SERVICES"  
	12          "PERSONAL SERVICES"             
	13          "ENTERTAINMENT AND REC. SERVICES"
	14          "PROFESSIONAL AND REL. SERVICES"
	15          "PUBLIC ADMINISTRATION"         
	16          "INDUSTRY NOT REPORTED"         
;
label values se2_in01 se2_in; 
label values se2_in02 se2_in; 
label values se2_in03 se2_in; 
label values se2_in04 se2_in; 
label values se2_in05 se2_in; 
label values se2_in06 se2_in; 
label values se2_in07 se2_in; 
label values se2_in08 se2_in; 
label values se2_in09 se2_in; 
label values se2_in10 se2_in; 
label values se2_in11 se2_in; 
label values se2_in12 se2_in; 
label values se2_in13 se2_in; 
label values se2_in14 se2_in; 
label values se2_in15 se2_in; 
label values se2_in16 se2_in; 
label values se2_in17 se2_in; 
label values se2_in18 se2_in; 
label values se2_in19 se2_in; 
label values se2_in20 se2_in; 
label values se2_in21 se2_in; 
label values se2_in22 se2_in; 
label values se2_in23 se2_in; 
label values se2_in24 se2_in; 
label values se2_in25 se2_in; 
label values se2_in26 se2_in; 
label values se2_in27 se2_in; 
label values se2_in28 se2_in; 
label values se2_in29 se2_in; 
label values se2_in30 se2_in; 
label values se2_in31 se2_in; 
label values se2_in32 se2_in; 
label define se2_in  
	1           "AGRICULTURE, FORESTRY, FISHERIES"
	2           "MINING"                        
	3           "CONSTRUCTION"                  
	4           "MANUFACTURING-NONDURABLE GOODS"
	5           "MANUFACTURING-DURABLE GOODS"   
	6           "TRANSPORTATION, COMM."         
	7           "WHOLESALE TRADE-DURABLE GOODS" 
	8           "WHOLESALE TRADE-NONDURABLE GOODS"
	9           "RETAIL TRADE"                  
	10          "FINANCE, INSURANCE, R.ESTATE"  
	11          "BUSINESS AND REPAIR SERVICES"  
	12          "PERSONAL SERVICES"             
	13          "ENTERTAINMENT AND REC. SERVICES"
	14          "PROFESSIONAL AND REL. SERVICES"
	15          "PUBLIC ADMINISTRATION"         
	16          "INDUSTRY NOT REPORTED"         
;
label values se1_wk01 se1_wk; 
label values se1_wk02 se1_wk; 
label values se1_wk03 se1_wk; 
label values se1_wk04 se1_wk; 
label values se1_wk05 se1_wk; 
label values se1_wk06 se1_wk; 
label values se1_wk07 se1_wk; 
label values se1_wk08 se1_wk; 
label values se1_wk09 se1_wk; 
label values se1_wk10 se1_wk; 
label values se1_wk11 se1_wk; 
label values se1_wk12 se1_wk; 
label values se1_wk13 se1_wk; 
label values se1_wk14 se1_wk; 
label values se1_wk15 se1_wk; 
label values se1_wk16 se1_wk; 
label values se1_wk17 se1_wk; 
label values se1_wk18 se1_wk; 
label values se1_wk19 se1_wk; 
label values se1_wk20 se1_wk; 
label values se1_wk21 se1_wk; 
label values se1_wk22 se1_wk; 
label values se1_wk23 se1_wk; 
label values se1_wk24 se1_wk; 
label values se1_wk25 se1_wk; 
label values se1_wk26 se1_wk; 
label values se1_wk27 se1_wk; 
label values se1_wk28 se1_wk; 
label values se1_wk29 se1_wk; 
label values se1_wk30 se1_wk; 
label values se1_wk31 se1_wk; 
label values se1_wk32 se1_wk; 
label define se1_wk  
	0           "NONE, NOT IN UNIVERSE, NOT IN" 
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS"                       
;
label values se2_wk01 se2_wk; 
label values se2_wk02 se2_wk; 
label values se2_wk03 se2_wk; 
label values se2_wk04 se2_wk; 
label values se2_wk05 se2_wk; 
label values se2_wk06 se2_wk; 
label values se2_wk07 se2_wk; 
label values se2_wk08 se2_wk; 
label values se2_wk09 se2_wk; 
label values se2_wk10 se2_wk; 
label values se2_wk11 se2_wk; 
label values se2_wk12 se2_wk; 
label values se2_wk13 se2_wk; 
label values se2_wk14 se2_wk; 
label values se2_wk15 se2_wk; 
label values se2_wk16 se2_wk; 
label values se2_wk17 se2_wk; 
label values se2_wk18 se2_wk; 
label values se2_wk19 se2_wk; 
label values se2_wk20 se2_wk; 
label values se2_wk21 se2_wk; 
label values se2_wk22 se2_wk; 
label values se2_wk23 se2_wk; 
label values se2_wk24 se2_wk; 
label values se2_wk25 se2_wk; 
label values se2_wk26 se2_wk; 
label values se2_wk27 se2_wk; 
label values se2_wk28 se2_wk; 
label values se2_wk29 se2_wk; 
label values se2_wk30 se2_wk; 
label values se2_wk31 se2_wk; 
label values se2_wk32 se2_wk; 
label define se2_wk  
	0           "NONE, NOT IN UNIVERSE, NOT IN" 
	1           "1 WEEK"                        
	2           "2 WEEKS"                       
	3           "3 WEEKS"                       
	4           "4 WEEKS"                       
	5           "5 WEEKS"                       
;
label values se1_hr01 se1_hr; 
label values se1_hr02 se1_hr; 
label values se1_hr03 se1_hr; 
label values se1_hr04 se1_hr; 
label values se1_hr05 se1_hr; 
label values se1_hr06 se1_hr; 
label values se1_hr07 se1_hr; 
label values se1_hr08 se1_hr; 
label values se1_hr09 se1_hr; 
label values se1_hr10 se1_hr; 
label values se1_hr11 se1_hr; 
label values se1_hr12 se1_hr; 
label values se1_hr13 se1_hr; 
label values se1_hr14 se1_hr; 
label values se1_hr15 se1_hr; 
label values se1_hr16 se1_hr; 
label values se1_hr17 se1_hr; 
label values se1_hr18 se1_hr; 
label values se1_hr19 se1_hr; 
label values se1_hr20 se1_hr; 
label values se1_hr21 se1_hr; 
label values se1_hr22 se1_hr; 
label values se1_hr23 se1_hr; 
label values se1_hr24 se1_hr; 
label values se1_hr25 se1_hr; 
label values se1_hr26 se1_hr; 
label values se1_hr27 se1_hr; 
label values se1_hr28 se1_hr; 
label values se1_hr29 se1_hr; 
label values se1_hr30 se1_hr; 
label values se1_hr31 se1_hr; 
label values se1_hr32 se1_hr; 
label define se1_hr  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values se2_hr01 se2_hr; 
label values se2_hr02 se2_hr; 
label values se2_hr03 se2_hr; 
label values se2_hr04 se2_hr; 
label values se2_hr05 se2_hr; 
label values se2_hr06 se2_hr; 
label values se2_hr07 se2_hr; 
label values se2_hr08 se2_hr; 
label values se2_hr09 se2_hr; 
label values se2_hr10 se2_hr; 
label values se2_hr11 se2_hr; 
label values se2_hr12 se2_hr; 
label values se2_hr13 se2_hr; 
label values se2_hr14 se2_hr; 
label values se2_hr15 se2_hr; 
label values se2_hr16 se2_hr; 
label values se2_hr17 se2_hr; 
label values se2_hr18 se2_hr; 
label values se2_hr19 se2_hr; 
label values se2_hr20 se2_hr; 
label values se2_hr21 se2_hr; 
label values se2_hr22 se2_hr; 
label values se2_hr23 se2_hr; 
label values se2_hr24 se2_hr; 
label values se2_hr25 se2_hr; 
label values se2_hr26 se2_hr; 
label values se2_hr27 se2_hr; 
label values se2_hr28 se2_hr; 
label values se2_hr29 se2_hr; 
label values se2_hr30 se2_hr; 
label values se2_hr31 se2_hr; 
label values se2_hr32 se2_hr; 
label define se2_hr  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values g1src1   g1src1l;
label define g1src1l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src2   g1src2l;
label define g1src2l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src3   g1src3l;
label define g1src3l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src4   g1src4l;
label define g1src4l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src5   g1src5l;
label define g1src5l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src6   g1src6l;
label define g1src6l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src7   g1src7l;
label define g1src7l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src8   g1src8l;
label define g1src8l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src9   g1src9l;
label define g1src9l 
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values g1src10  g1src10l;
label define g1src10l
	0           "NOT APPLICABLE, NOT IN"        
	1           "SOCIAL SECURITY"               
	2           "RAILROAD RETIREMENT"           
	3           "FEDERAL SUPPLEMENTAL SECURITY" 
	5           "STATE UNEMPLOYMENT"            
	6           "SUPPLEMENTAL UNEMPLOYMENT"     
	7           "OTHER UNEMPLOYMENT"            
	8           "VETERANS COMPENSATION OR"      
	10          "WORKERS COMPENSATION"          
	12          "EMPLOYER OR UNION TEMPORARY"   
	13          "PAYMENTS FROM A SICKNESS,"     
	20          "AID TO FAMILIES WITH"          
	21          "GENERAL ASSISTANCE OR GENERAL" 
	23          "FOSTER CHILD CARE PAYMENTS"    
	24          "OTHER WELFARE"                 
	25          "WIC"                           
	27          "FOOD STAMPS"                   
	28          "CHILD SUPPORT PAYMENTS"        
	29          "ALIMONY PAYMENTS"              
	30          "PENSION FROM COMPANY OR UNION" 
	31          "FEDERAL CIVIL SERVICE OR"      
	32          "U.S. MILITARY RETIREMENT PAY"  
	34          "STATE GOVERNMENT PENSIONS"     
	35          "LOCAL GOVERNMENT PENSIONS"     
	36          "INCOME FROM PAID UP LIFE"      
	37          "ESTATES AND TRUSTS"            
	38          "OTHER PAYMENTS FOR"            
	40          "GI BILL EDUCATION BENEFITS"    
	41          "OTHER VA EDUCATIONAL"          
	50          "INCOME ASSISTANCE FROM A"      
	51          "MONEY FROM RELATIVES OR"       
	52          "LUMP SUM PAYMENTS"             
	53          "INCOME FROM ROOMERS OR"        
	54          "NATIONAL GUARD OR RESERVE PAY" 
	55          "INCIDENTAL OR CASUAL EARNINGS" 
	56          "OTHER CASH INCOME NOT"         
	75          "STATE SSI/BLACK LUNG/STATE"    
;
label values ssrecin1 ssrecin;
label values ssrecin2 ssrecin;
label values ssrecin3 ssrecin;
label values ssrecin4 ssrecin;
label values ssrecin5 ssrecin;
label values ssrecin6 ssrecin;
label values ssrecin7 ssrecin;
label values ssrecin8 ssrecin;
label define ssrecin 
	0           "NOT IN UNIVERSE"               
	1           "ADULT BENEFITS RECEIVED IN"    
	2           "ONLY ADULT BENEFITS RECEIVED"  
	3           "ONLY CHILD BENEFITS RECEIVED"  
	4           "ADULT BENEFITS RECEIVED IN"    
	5           "ADULT BENEFITS RECEIVED"       
;
label values rrrecin1 rrrecin;
label values rrrecin2 rrrecin;
label values rrrecin3 rrrecin;
label values rrrecin4 rrrecin;
label values rrrecin5 rrrecin;
label values rrrecin6 rrrecin;
label values rrrecin7 rrrecin;
label values rrrecin8 rrrecin;
label define rrrecin 
	0           "NOT IN UNIVERSE"               
	1           "ADULT BENEFITS RECEIVED IN"    
	2           "ONLY ADULT BENEFITS RECEIVED"  
	3           "ONLY CHILD BENEFITS RECEIVED"  
	4           "ADULT BENEFITS RECEIVED IN"    
	5           "ADULT BENEFITS RECEIVED"       
;
label values sc3060   sc3060l;
label define sc3060l 
	-1          "DON'T KNOW"                    
	0           "NOT IN UNIVERSE OR DON'T KNOW" 
	1           "YES"                           
	2           "NO"                            
;
label values ast1001  ast100l;
label values ast1002  ast100l;
label values ast1003  ast100l;
label values ast1004  ast100l;
label values ast1005  ast100l;
label values ast1006  ast100l;
label values ast1007  ast100l;
label values ast1008  ast100l;
label define ast100l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1011  ast101l;
label values ast1012  ast101l;
label values ast1013  ast101l;
label values ast1014  ast101l;
label values ast1015  ast101l;
label values ast1016  ast101l;
label values ast1017  ast101l;
label values ast1018  ast101l;
label define ast101l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1021  ast102l;
label values ast1022  ast102l;
label values ast1023  ast102l;
label values ast1024  ast102l;
label values ast1025  ast102l;
label values ast1026  ast102l;
label values ast1027  ast102l;
label values ast1028  ast102l;
label define ast102l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1031  ast103l;
label values ast1032  ast103l;
label values ast1033  ast103l;
label values ast1034  ast103l;
label values ast1035  ast103l;
label values ast1036  ast103l;
label values ast1037  ast103l;
label values ast1038  ast103l;
label define ast103l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1041  ast104l;
label values ast1042  ast104l;
label values ast1043  ast104l;
label values ast1044  ast104l;
label values ast1045  ast104l;
label values ast1046  ast104l;
label values ast1047  ast104l;
label values ast1048  ast104l;
label define ast104l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1051  ast105l;
label values ast1052  ast105l;
label values ast1053  ast105l;
label values ast1054  ast105l;
label values ast1055  ast105l;
label values ast1056  ast105l;
label values ast1057  ast105l;
label values ast1058  ast105l;
label define ast105l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1061  ast106l;
label values ast1062  ast106l;
label values ast1063  ast106l;
label values ast1064  ast106l;
label values ast1065  ast106l;
label values ast1066  ast106l;
label values ast1067  ast106l;
label values ast1068  ast106l;
label define ast106l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1071  ast107l;
label values ast1072  ast107l;
label values ast1073  ast107l;
label values ast1074  ast107l;
label values ast1075  ast107l;
label values ast1076  ast107l;
label values ast1077  ast107l;
label values ast1078  ast107l;
label define ast107l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1101  ast110l;
label values ast1102  ast110l;
label values ast1103  ast110l;
label values ast1104  ast110l;
label values ast1105  ast110l;
label values ast1106  ast110l;
label values ast1107  ast110l;
label values ast1108  ast110l;
label define ast110l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1201  ast120l;
label values ast1202  ast120l;
label values ast1203  ast120l;
label values ast1204  ast120l;
label values ast1205  ast120l;
label values ast1206  ast120l;
label values ast1207  ast120l;
label values ast1208  ast120l;
label define ast120l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1301  ast130l;
label values ast1302  ast130l;
label values ast1303  ast130l;
label values ast1304  ast130l;
label values ast1305  ast130l;
label values ast1306  ast130l;
label values ast1307  ast130l;
label values ast1308  ast130l;
label define ast130l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1401  ast140l;
label values ast1402  ast140l;
label values ast1403  ast140l;
label values ast1404  ast140l;
label values ast1405  ast140l;
label values ast1406  ast140l;
label values ast1407  ast140l;
label values ast1408  ast140l;
label define ast140l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values ast1501  ast150l;
label values ast1502  ast150l;
label values ast1503  ast150l;
label values ast1504  ast150l;
label values ast1505  ast150l;
label values ast1506  ast150l;
label values ast1507  ast150l;
label values ast1508  ast150l;
label define ast150l 
	0           "NOT APPLICABLE"                
	1           "YES"                           
	2           "NO"                            
;
label values g2src100 g2src10n;
label define g2src10n
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values g2src104 g2src10k;
label define g2src10k
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values g2src110 g2src11n;
label define g2src11n
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values g2src120 g2src12n;
label define g2src12n
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values g2src130 g2src13n;
label define g2src13n
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values g2src140 g2src14n;
label define g2src14n
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values careco01 careco; 
label values careco02 careco; 
label values careco03 careco; 
label values careco04 careco; 
label values careco05 careco; 
label values careco06 careco; 
label values careco07 careco; 
label values careco08 careco; 
label values careco09 careco; 
label values careco10 careco; 
label values careco11 careco; 
label values careco12 careco; 
label values careco13 careco; 
label values careco14 careco; 
label values careco15 careco; 
label values careco16 careco; 
label values careco17 careco; 
label values careco18 careco; 
label values careco19 careco; 
label values careco20 careco; 
label values careco21 careco; 
label values careco22 careco; 
label values careco23 careco; 
label values careco24 careco; 
label values careco25 careco; 
label values careco26 careco; 
label values careco27 careco; 
label values careco28 careco; 
label values careco29 careco; 
label values careco30 careco; 
label values careco31 careco; 
label values careco32 careco; 
label define careco  
	0           "NOT APPLICABLE IF AGE UNDER"   
	1           "YES"                           
	2           "NO"                            
;
label values caidco01 caidco; 
label values caidco02 caidco; 
label values caidco03 caidco; 
label values caidco04 caidco; 
label values caidco05 caidco; 
label values caidco06 caidco; 
label values caidco07 caidco; 
label values caidco08 caidco; 
label values caidco09 caidco; 
label values caidco10 caidco; 
label values caidco11 caidco; 
label values caidco12 caidco; 
label values caidco13 caidco; 
label values caidco14 caidco; 
label values caidco15 caidco; 
label values caidco16 caidco; 
label values caidco17 caidco; 
label values caidco18 caidco; 
label values caidco19 caidco; 
label values caidco20 caidco; 
label values caidco21 caidco; 
label values caidco22 caidco; 
label values caidco23 caidco; 
label values caidco24 caidco; 
label values caidco25 caidco; 
label values caidco26 caidco; 
label values caidco27 caidco; 
label values caidco28 caidco; 
label values caidco29 caidco; 
label values caidco30 caidco; 
label values caidco31 caidco; 
label values caidco32 caidco; 
label define caidco  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values wiccov01 wiccov; 
label values wiccov02 wiccov; 
label values wiccov03 wiccov; 
label values wiccov04 wiccov; 
label values wiccov05 wiccov; 
label values wiccov06 wiccov; 
label values wiccov07 wiccov; 
label values wiccov08 wiccov; 
label values wiccov09 wiccov; 
label values wiccov10 wiccov; 
label values wiccov11 wiccov; 
label values wiccov12 wiccov; 
label values wiccov13 wiccov; 
label values wiccov14 wiccov; 
label values wiccov15 wiccov; 
label values wiccov16 wiccov; 
label values wiccov17 wiccov; 
label values wiccov18 wiccov; 
label values wiccov19 wiccov; 
label values wiccov20 wiccov; 
label values wiccov21 wiccov; 
label values wiccov22 wiccov; 
label values wiccov23 wiccov; 
label values wiccov24 wiccov; 
label values wiccov25 wiccov; 
label values wiccov26 wiccov; 
label values wiccov27 wiccov; 
label values wiccov28 wiccov; 
label values wiccov29 wiccov; 
label values wiccov30 wiccov; 
label values wiccov31 wiccov; 
label values wiccov32 wiccov; 
label define wiccov  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values afdc_01  afdc;   
label values afdc_02  afdc;   
label values afdc_03  afdc;   
label values afdc_04  afdc;   
label values afdc_05  afdc;   
label values afdc_06  afdc;   
label values afdc_07  afdc;   
label values afdc_08  afdc;   
label values afdc_09  afdc;   
label values afdc_10  afdc;   
label values afdc_11  afdc;   
label values afdc_12  afdc;   
label values afdc_13  afdc;   
label values afdc_14  afdc;   
label values afdc_15  afdc;   
label values afdc_16  afdc;   
label values afdc_17  afdc;   
label values afdc_18  afdc;   
label values afdc_19  afdc;   
label values afdc_20  afdc;   
label values afdc_21  afdc;   
label values afdc_22  afdc;   
label values afdc_23  afdc;   
label values afdc_24  afdc;   
label values afdc_25  afdc;   
label values afdc_26  afdc;   
label values afdc_27  afdc;   
label values afdc_28  afdc;   
label values afdc_29  afdc;   
label values afdc_30  afdc;   
label values afdc_31  afdc;   
label values afdc_32  afdc;   
label define afdc    
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values foodst01 foodst; 
label values foodst02 foodst; 
label values foodst03 foodst; 
label values foodst04 foodst; 
label values foodst05 foodst; 
label values foodst06 foodst; 
label values foodst07 foodst; 
label values foodst08 foodst; 
label values foodst09 foodst; 
label values foodst10 foodst; 
label values foodst11 foodst; 
label values foodst12 foodst; 
label values foodst13 foodst; 
label values foodst14 foodst; 
label values foodst15 foodst; 
label values foodst16 foodst; 
label values foodst17 foodst; 
label values foodst18 foodst; 
label values foodst19 foodst; 
label values foodst20 foodst; 
label values foodst21 foodst; 
label values foodst22 foodst; 
label values foodst23 foodst; 
label values foodst24 foodst; 
label values foodst25 foodst; 
label values foodst26 foodst; 
label values foodst27 foodst; 
label values foodst28 foodst; 
label values foodst29 foodst; 
label values foodst30 foodst; 
label values foodst31 foodst; 
label values foodst32 foodst; 
label define foodst  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values gen_as01 gen_as; 
label values gen_as02 gen_as; 
label values gen_as03 gen_as; 
label values gen_as04 gen_as; 
label values gen_as05 gen_as; 
label values gen_as06 gen_as; 
label values gen_as07 gen_as; 
label values gen_as08 gen_as; 
label values gen_as09 gen_as; 
label values gen_as10 gen_as; 
label values gen_as11 gen_as; 
label values gen_as12 gen_as; 
label values gen_as13 gen_as; 
label values gen_as14 gen_as; 
label values gen_as15 gen_as; 
label values gen_as16 gen_as; 
label values gen_as17 gen_as; 
label values gen_as18 gen_as; 
label values gen_as19 gen_as; 
label values gen_as20 gen_as; 
label values gen_as21 gen_as; 
label values gen_as22 gen_as; 
label values gen_as23 gen_as; 
label values gen_as24 gen_as; 
label values gen_as25 gen_as; 
label values gen_as26 gen_as; 
label values gen_as27 gen_as; 
label values gen_as28 gen_as; 
label values gen_as29 gen_as; 
label values gen_as30 gen_as; 
label values gen_as31 gen_as; 
label values gen_as32 gen_as; 
label define gen_as  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values fost_k01 fost_k; 
label values fost_k02 fost_k; 
label values fost_k03 fost_k; 
label values fost_k04 fost_k; 
label values fost_k05 fost_k; 
label values fost_k06 fost_k; 
label values fost_k07 fost_k; 
label values fost_k08 fost_k; 
label values fost_k09 fost_k; 
label values fost_k10 fost_k; 
label values fost_k11 fost_k; 
label values fost_k12 fost_k; 
label values fost_k13 fost_k; 
label values fost_k14 fost_k; 
label values fost_k15 fost_k; 
label values fost_k16 fost_k; 
label values fost_k17 fost_k; 
label values fost_k18 fost_k; 
label values fost_k19 fost_k; 
label values fost_k20 fost_k; 
label values fost_k21 fost_k; 
label values fost_k22 fost_k; 
label values fost_k23 fost_k; 
label values fost_k24 fost_k; 
label values fost_k25 fost_k; 
label values fost_k26 fost_k; 
label values fost_k27 fost_k; 
label values fost_k28 fost_k; 
label values fost_k29 fost_k; 
label values fost_k30 fost_k; 
label values fost_k31 fost_k; 
label values fost_k32 fost_k; 
label define fost_k  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values oth_we01 oth_we; 
label values oth_we02 oth_we; 
label values oth_we03 oth_we; 
label values oth_we04 oth_we; 
label values oth_we05 oth_we; 
label values oth_we06 oth_we; 
label values oth_we07 oth_we; 
label values oth_we08 oth_we; 
label values oth_we09 oth_we; 
label values oth_we10 oth_we; 
label values oth_we11 oth_we; 
label values oth_we12 oth_we; 
label values oth_we13 oth_we; 
label values oth_we14 oth_we; 
label values oth_we15 oth_we; 
label values oth_we16 oth_we; 
label values oth_we17 oth_we; 
label values oth_we18 oth_we; 
label values oth_we19 oth_we; 
label values oth_we20 oth_we; 
label values oth_we21 oth_we; 
label values oth_we22 oth_we; 
label values oth_we23 oth_we; 
label values oth_we24 oth_we; 
label values oth_we25 oth_we; 
label values oth_we26 oth_we; 
label values oth_we27 oth_we; 
label values oth_we28 oth_we; 
label values oth_we29 oth_we; 
label values oth_we30 oth_we; 
label values oth_we31 oth_we; 
label values oth_we32 oth_we; 
label define oth_we  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values soc_se01 soc_se; 
label values soc_se02 soc_se; 
label values soc_se03 soc_se; 
label values soc_se04 soc_se; 
label values soc_se05 soc_se; 
label values soc_se06 soc_se; 
label values soc_se07 soc_se; 
label values soc_se08 soc_se; 
label values soc_se09 soc_se; 
label values soc_se10 soc_se; 
label values soc_se11 soc_se; 
label values soc_se12 soc_se; 
label values soc_se13 soc_se; 
label values soc_se14 soc_se; 
label values soc_se15 soc_se; 
label values soc_se16 soc_se; 
label values soc_se17 soc_se; 
label values soc_se18 soc_se; 
label values soc_se19 soc_se; 
label values soc_se20 soc_se; 
label values soc_se21 soc_se; 
label values soc_se22 soc_se; 
label values soc_se23 soc_se; 
label values soc_se24 soc_se; 
label values soc_se25 soc_se; 
label values soc_se26 soc_se; 
label values soc_se27 soc_se; 
label values soc_se28 soc_se; 
label values soc_se29 soc_se; 
label values soc_se30 soc_se; 
label values soc_se31 soc_se; 
label values soc_se32 soc_se; 
label define soc_se  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values railro01 railro; 
label values railro02 railro; 
label values railro03 railro; 
label values railro04 railro; 
label values railro05 railro; 
label values railro06 railro; 
label values railro07 railro; 
label values railro08 railro; 
label values railro09 railro; 
label values railro10 railro; 
label values railro11 railro; 
label values railro12 railro; 
label values railro13 railro; 
label values railro14 railro; 
label values railro15 railro; 
label values railro16 railro; 
label values railro17 railro; 
label values railro18 railro; 
label values railro19 railro; 
label values railro20 railro; 
label values railro21 railro; 
label values railro22 railro; 
label values railro23 railro; 
label values railro24 railro; 
label values railro25 railro; 
label values railro26 railro; 
label values railro27 railro; 
label values railro28 railro; 
label values railro29 railro; 
label values railro30 railro; 
label values railro31 railro; 
label values railro32 railro; 
label define railro  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values vets_01  vets;   
label values vets_02  vets;   
label values vets_03  vets;   
label values vets_04  vets;   
label values vets_05  vets;   
label values vets_06  vets;   
label values vets_07  vets;   
label values vets_08  vets;   
label values vets_09  vets;   
label values vets_10  vets;   
label values vets_11  vets;   
label values vets_12  vets;   
label values vets_13  vets;   
label values vets_14  vets;   
label values vets_15  vets;   
label values vets_16  vets;   
label values vets_17  vets;   
label values vets_18  vets;   
label values vets_19  vets;   
label values vets_20  vets;   
label values vets_21  vets;   
label values vets_22  vets;   
label values vets_23  vets;   
label values vets_24  vets;   
label values vets_25  vets;   
label values vets_26  vets;   
label values vets_27  vets;   
label values vets_28  vets;   
label values vets_29  vets;   
label values vets_30  vets;   
label values vets_31  vets;   
label values vets_32  vets;   
label define vets    
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values champu01 champu; 
label values champu02 champu; 
label values champu03 champu; 
label values champu04 champu; 
label values champu05 champu; 
label values champu06 champu; 
label values champu07 champu; 
label values champu08 champu; 
label values champu09 champu; 
label values champu10 champu; 
label values champu11 champu; 
label values champu12 champu; 
label values champu13 champu; 
label values champu14 champu; 
label values champu15 champu; 
label values champu16 champu; 
label values champu17 champu; 
label values champu18 champu; 
label values champu19 champu; 
label values champu20 champu; 
label values champu21 champu; 
label values champu22 champu; 
label values champu23 champu; 
label values champu24 champu; 
label values champu25 champu; 
label values champu26 champu; 
label values champu27 champu; 
label values champu28 champu; 
label values champu29 champu; 
label values champu30 champu; 
label values champu31 champu; 
label values champu32 champu; 
label define champu  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values champv01 champv; 
label values champv02 champv; 
label values champv03 champv; 
label values champv04 champv; 
label values champv05 champv; 
label values champv06 champv; 
label values champv07 champv; 
label values champv08 champv; 
label values champv09 champv; 
label values champv10 champv; 
label values champv11 champv; 
label values champv12 champv; 
label values champv13 champv; 
label values champv14 champv; 
label values champv15 champv; 
label values champv16 champv; 
label values champv17 champv; 
label values champv18 champv; 
label values champv19 champv; 
label values champv20 champv; 
label values champv21 champv; 
label values champv22 champv; 
label values champv23 champv; 
label values champv24 champv; 
label values champv25 champv; 
label values champv26 champv; 
label values champv27 champv; 
label values champv28 champv; 
label values champv29 champv; 
label values champv30 champv; 
label values champv31 champv; 
label values champv32 champv; 
label define champv  
	0           "NOT APPLICABLE, NOT IN"        
	1           "YES"                           
	2           "NO"                            
;
label values hiownc01 hiownc; 
label values hiownc02 hiownc; 
label values hiownc03 hiownc; 
label values hiownc04 hiownc; 
label values hiownc05 hiownc; 
label values hiownc06 hiownc; 
label values hiownc07 hiownc; 
label values hiownc08 hiownc; 
label values hiownc09 hiownc; 
label values hiownc10 hiownc; 
label values hiownc11 hiownc; 
label values hiownc12 hiownc; 
label values hiownc13 hiownc; 
label values hiownc14 hiownc; 
label values hiownc15 hiownc; 
label values hiownc16 hiownc; 
label values hiownc17 hiownc; 
label values hiownc18 hiownc; 
label values hiownc19 hiownc; 
label values hiownc20 hiownc; 
label values hiownc21 hiownc; 
label values hiownc22 hiownc; 
label values hiownc23 hiownc; 
label values hiownc24 hiownc; 
label values hiownc25 hiownc; 
label values hiownc26 hiownc; 
label values hiownc27 hiownc; 
label values hiownc28 hiownc; 
label values hiownc29 hiownc; 
label values hiownc30 hiownc; 
label values hiownc31 hiownc; 
label values hiownc32 hiownc; 
label define hiownc  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "HAD HEALTH INSURANCE IN OWN"   
	2           "DID NOT HAVE HEALTH INSURANCE" 
;
label values hi_otc01 hi_otc; 
label values hi_otc02 hi_otc; 
label values hi_otc03 hi_otc; 
label values hi_otc04 hi_otc; 
label values hi_otc05 hi_otc; 
label values hi_otc06 hi_otc; 
label values hi_otc07 hi_otc; 
label values hi_otc08 hi_otc; 
label values hi_otc09 hi_otc; 
label values hi_otc10 hi_otc; 
label values hi_otc11 hi_otc; 
label values hi_otc12 hi_otc; 
label values hi_otc13 hi_otc; 
label values hi_otc14 hi_otc; 
label values hi_otc15 hi_otc; 
label values hi_otc16 hi_otc; 
label values hi_otc17 hi_otc; 
label values hi_otc18 hi_otc; 
label values hi_otc19 hi_otc; 
label values hi_otc20 hi_otc; 
label values hi_otc21 hi_otc; 
label values hi_otc22 hi_otc; 
label values hi_otc23 hi_otc; 
label values hi_otc24 hi_otc; 
label values hi_otc25 hi_otc; 
label values hi_otc26 hi_otc; 
label values hi_otc27 hi_otc; 
label values hi_otc28 hi_otc; 
label values hi_otc29 hi_otc; 
label values hi_otc30 hi_otc; 
label values hi_otc31 hi_otc; 
label values hi_otc32 hi_otc; 
label define hi_otc  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "HAD HEALTH INSURANCE THRU"     
	2           "DID NOT HAVE HEALTH INSURANCE" 
;
label values hiempl01 hiempl; 
label values hiempl02 hiempl; 
label values hiempl03 hiempl; 
label values hiempl04 hiempl; 
label values hiempl05 hiempl; 
label values hiempl06 hiempl; 
label values hiempl07 hiempl; 
label values hiempl08 hiempl; 
label values hiempl09 hiempl; 
label values hiempl10 hiempl; 
label values hiempl11 hiempl; 
label values hiempl12 hiempl; 
label values hiempl13 hiempl; 
label values hiempl14 hiempl; 
label values hiempl15 hiempl; 
label values hiempl16 hiempl; 
label values hiempl17 hiempl; 
label values hiempl18 hiempl; 
label values hiempl19 hiempl; 
label values hiempl20 hiempl; 
label values hiempl21 hiempl; 
label values hiempl22 hiempl; 
label values hiempl23 hiempl; 
label values hiempl24 hiempl; 
label values hiempl25 hiempl; 
label values hiempl26 hiempl; 
label values hiempl27 hiempl; 
label values hiempl28 hiempl; 
label values hiempl29 hiempl; 
label values hiempl30 hiempl; 
label values hiempl31 hiempl; 
label values hiempl32 hiempl; 
label define hiempl  
	0           "NOT IN UNIVERSE, NOT IN"       
	1           "HEALTH INSURANCE COVERAGE"     
	2           "HEALTH INSURANCE COVERAGE NOT" 
;
label values ss_pid01 ss_pid; 
label values ss_pid02 ss_pid; 
label values ss_pid03 ss_pid; 
label values ss_pid04 ss_pid; 
label values ss_pid05 ss_pid; 
label values ss_pid06 ss_pid; 
label values ss_pid07 ss_pid; 
label values ss_pid08 ss_pid; 
label values ss_pid09 ss_pid; 
label values ss_pid10 ss_pid; 
label values ss_pid11 ss_pid; 
label values ss_pid12 ss_pid; 
label values ss_pid13 ss_pid; 
label values ss_pid14 ss_pid; 
label values ss_pid15 ss_pid; 
label values ss_pid16 ss_pid; 
label values ss_pid17 ss_pid; 
label values ss_pid18 ss_pid; 
label values ss_pid19 ss_pid; 
label values ss_pid20 ss_pid; 
label values ss_pid21 ss_pid; 
label values ss_pid22 ss_pid; 
label values ss_pid23 ss_pid; 
label values ss_pid24 ss_pid; 
label values ss_pid25 ss_pid; 
label values ss_pid26 ss_pid; 
label values ss_pid27 ss_pid; 
label values ss_pid28 ss_pid; 
label values ss_pid29 ss_pid; 
label values ss_pid30 ss_pid; 
label values ss_pid31 ss_pid; 
label values ss_pid32 ss_pid; 
label define ss_pid  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values rr_pid01 rr_pid; 
label values rr_pid02 rr_pid; 
label values rr_pid03 rr_pid; 
label values rr_pid04 rr_pid; 
label values rr_pid05 rr_pid; 
label values rr_pid06 rr_pid; 
label values rr_pid07 rr_pid; 
label values rr_pid08 rr_pid; 
label values rr_pid09 rr_pid; 
label values rr_pid10 rr_pid; 
label values rr_pid11 rr_pid; 
label values rr_pid12 rr_pid; 
label values rr_pid13 rr_pid; 
label values rr_pid14 rr_pid; 
label values rr_pid15 rr_pid; 
label values rr_pid16 rr_pid; 
label values rr_pid17 rr_pid; 
label values rr_pid18 rr_pid; 
label values rr_pid19 rr_pid; 
label values rr_pid20 rr_pid; 
label values rr_pid21 rr_pid; 
label values rr_pid22 rr_pid; 
label values rr_pid23 rr_pid; 
label values rr_pid24 rr_pid; 
label values rr_pid25 rr_pid; 
label values rr_pid26 rr_pid; 
label values rr_pid27 rr_pid; 
label values rr_pid28 rr_pid; 
label values rr_pid29 rr_pid; 
label values rr_pid30 rr_pid; 
label values rr_pid31 rr_pid; 
label values rr_pid32 rr_pid; 
label define rr_pid  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values va_pid01 va_pid; 
label values va_pid02 va_pid; 
label values va_pid03 va_pid; 
label values va_pid04 va_pid; 
label values va_pid05 va_pid; 
label values va_pid06 va_pid; 
label values va_pid07 va_pid; 
label values va_pid08 va_pid; 
label values va_pid09 va_pid; 
label values va_pid10 va_pid; 
label values va_pid11 va_pid; 
label values va_pid12 va_pid; 
label values va_pid13 va_pid; 
label values va_pid14 va_pid; 
label values va_pid15 va_pid; 
label values va_pid16 va_pid; 
label values va_pid17 va_pid; 
label values va_pid18 va_pid; 
label values va_pid19 va_pid; 
label values va_pid20 va_pid; 
label values va_pid21 va_pid; 
label values va_pid22 va_pid; 
label values va_pid23 va_pid; 
label values va_pid24 va_pid; 
label values va_pid25 va_pid; 
label values va_pid26 va_pid; 
label values va_pid27 va_pid; 
label values va_pid28 va_pid; 
label values va_pid29 va_pid; 
label values va_pid30 va_pid; 
label values va_pid31 va_pid; 
label values va_pid32 va_pid; 
label define va_pid  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values afdcpi01 afdcpi; 
label values afdcpi02 afdcpi; 
label values afdcpi03 afdcpi; 
label values afdcpi04 afdcpi; 
label values afdcpi05 afdcpi; 
label values afdcpi06 afdcpi; 
label values afdcpi07 afdcpi; 
label values afdcpi08 afdcpi; 
label values afdcpi09 afdcpi; 
label values afdcpi10 afdcpi; 
label values afdcpi11 afdcpi; 
label values afdcpi12 afdcpi; 
label values afdcpi13 afdcpi; 
label values afdcpi14 afdcpi; 
label values afdcpi15 afdcpi; 
label values afdcpi16 afdcpi; 
label values afdcpi17 afdcpi; 
label values afdcpi18 afdcpi; 
label values afdcpi19 afdcpi; 
label values afdcpi20 afdcpi; 
label values afdcpi21 afdcpi; 
label values afdcpi22 afdcpi; 
label values afdcpi23 afdcpi; 
label values afdcpi24 afdcpi; 
label values afdcpi25 afdcpi; 
label values afdcpi26 afdcpi; 
label values afdcpi27 afdcpi; 
label values afdcpi28 afdcpi; 
label values afdcpi29 afdcpi; 
label values afdcpi30 afdcpi; 
label values afdcpi31 afdcpi; 
label values afdcpi32 afdcpi; 
label define afdcpi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ga_pid01 ga_pid; 
label values ga_pid02 ga_pid; 
label values ga_pid03 ga_pid; 
label values ga_pid04 ga_pid; 
label values ga_pid05 ga_pid; 
label values ga_pid06 ga_pid; 
label values ga_pid07 ga_pid; 
label values ga_pid08 ga_pid; 
label values ga_pid09 ga_pid; 
label values ga_pid10 ga_pid; 
label values ga_pid11 ga_pid; 
label values ga_pid12 ga_pid; 
label values ga_pid13 ga_pid; 
label values ga_pid14 ga_pid; 
label values ga_pid15 ga_pid; 
label values ga_pid16 ga_pid; 
label values ga_pid17 ga_pid; 
label values ga_pid18 ga_pid; 
label values ga_pid19 ga_pid; 
label values ga_pid20 ga_pid; 
label values ga_pid21 ga_pid; 
label values ga_pid22 ga_pid; 
label values ga_pid23 ga_pid; 
label values ga_pid24 ga_pid; 
label values ga_pid25 ga_pid; 
label values ga_pid26 ga_pid; 
label values ga_pid27 ga_pid; 
label values ga_pid28 ga_pid; 
label values ga_pid29 ga_pid; 
label values ga_pid30 ga_pid; 
label values ga_pid31 ga_pid; 
label values ga_pid32 ga_pid; 
label define ga_pid  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values fostpi01 fostpi; 
label values fostpi02 fostpi; 
label values fostpi03 fostpi; 
label values fostpi04 fostpi; 
label values fostpi05 fostpi; 
label values fostpi06 fostpi; 
label values fostpi07 fostpi; 
label values fostpi08 fostpi; 
label values fostpi09 fostpi; 
label values fostpi10 fostpi; 
label values fostpi11 fostpi; 
label values fostpi12 fostpi; 
label values fostpi13 fostpi; 
label values fostpi14 fostpi; 
label values fostpi15 fostpi; 
label values fostpi16 fostpi; 
label values fostpi17 fostpi; 
label values fostpi18 fostpi; 
label values fostpi19 fostpi; 
label values fostpi20 fostpi; 
label values fostpi21 fostpi; 
label values fostpi22 fostpi; 
label values fostpi23 fostpi; 
label values fostpi24 fostpi; 
label values fostpi25 fostpi; 
label values fostpi26 fostpi; 
label values fostpi27 fostpi; 
label values fostpi28 fostpi; 
label values fostpi29 fostpi; 
label values fostpi30 fostpi; 
label values fostpi31 fostpi; 
label values fostpi32 fostpi; 
label define fostpi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values oth_pi01 oth_pi; 
label values oth_pi02 oth_pi; 
label values oth_pi03 oth_pi; 
label values oth_pi04 oth_pi; 
label values oth_pi05 oth_pi; 
label values oth_pi06 oth_pi; 
label values oth_pi07 oth_pi; 
label values oth_pi08 oth_pi; 
label values oth_pi09 oth_pi; 
label values oth_pi10 oth_pi; 
label values oth_pi11 oth_pi; 
label values oth_pi12 oth_pi; 
label values oth_pi13 oth_pi; 
label values oth_pi14 oth_pi; 
label values oth_pi15 oth_pi; 
label values oth_pi16 oth_pi; 
label values oth_pi17 oth_pi; 
label values oth_pi18 oth_pi; 
label values oth_pi19 oth_pi; 
label values oth_pi20 oth_pi; 
label values oth_pi21 oth_pi; 
label values oth_pi22 oth_pi; 
label values oth_pi23 oth_pi; 
label values oth_pi24 oth_pi; 
label values oth_pi25 oth_pi; 
label values oth_pi26 oth_pi; 
label values oth_pi27 oth_pi; 
label values oth_pi28 oth_pi; 
label values oth_pi29 oth_pi; 
label values oth_pi30 oth_pi; 
label values oth_pi31 oth_pi; 
label values oth_pi32 oth_pi; 
label define oth_pi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values wic_pi01 wic_pi; 
label values wic_pi02 wic_pi; 
label values wic_pi03 wic_pi; 
label values wic_pi04 wic_pi; 
label values wic_pi05 wic_pi; 
label values wic_pi06 wic_pi; 
label values wic_pi07 wic_pi; 
label values wic_pi08 wic_pi; 
label values wic_pi09 wic_pi; 
label values wic_pi10 wic_pi; 
label values wic_pi11 wic_pi; 
label values wic_pi12 wic_pi; 
label values wic_pi13 wic_pi; 
label values wic_pi14 wic_pi; 
label values wic_pi15 wic_pi; 
label values wic_pi16 wic_pi; 
label values wic_pi17 wic_pi; 
label values wic_pi18 wic_pi; 
label values wic_pi19 wic_pi; 
label values wic_pi20 wic_pi; 
label values wic_pi21 wic_pi; 
label values wic_pi22 wic_pi; 
label values wic_pi23 wic_pi; 
label values wic_pi24 wic_pi; 
label values wic_pi25 wic_pi; 
label values wic_pi26 wic_pi; 
label values wic_pi27 wic_pi; 
label values wic_pi28 wic_pi; 
label values wic_pi29 wic_pi; 
label values wic_pi30 wic_pi; 
label values wic_pi31 wic_pi; 
label values wic_pi32 wic_pi; 
label define wic_pi  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values fs_pid01 fs_pid; 
label values fs_pid02 fs_pid; 
label values fs_pid03 fs_pid; 
label values fs_pid04 fs_pid; 
label values fs_pid05 fs_pid; 
label values fs_pid06 fs_pid; 
label values fs_pid07 fs_pid; 
label values fs_pid08 fs_pid; 
label values fs_pid09 fs_pid; 
label values fs_pid10 fs_pid; 
label values fs_pid11 fs_pid; 
label values fs_pid12 fs_pid; 
label values fs_pid13 fs_pid; 
label values fs_pid14 fs_pid; 
label values fs_pid15 fs_pid; 
label values fs_pid16 fs_pid; 
label values fs_pid17 fs_pid; 
label values fs_pid18 fs_pid; 
label values fs_pid19 fs_pid; 
label values fs_pid20 fs_pid; 
label values fs_pid21 fs_pid; 
label values fs_pid22 fs_pid; 
label values fs_pid23 fs_pid; 
label values fs_pid24 fs_pid; 
label values fs_pid25 fs_pid; 
label values fs_pid26 fs_pid; 
label values fs_pid27 fs_pid; 
label values fs_pid28 fs_pid; 
label values fs_pid29 fs_pid; 
label values fs_pid30 fs_pid; 
label values fs_pid31 fs_pid; 
label values fs_pid32 fs_pid; 
label define fs_pid  
	0           "NOT IN UNIVERSE, NOT IN"       
;
label values ws1_im01 ws1_im; 
label values ws1_im02 ws1_im; 
label values ws1_im03 ws1_im; 
label values ws1_im04 ws1_im; 
label values ws1_im05 ws1_im; 
label values ws1_im06 ws1_im; 
label values ws1_im07 ws1_im; 
label values ws1_im08 ws1_im; 
label values ws1_im09 ws1_im; 
label values ws1_im10 ws1_im; 
label values ws1_im11 ws1_im; 
label values ws1_im12 ws1_im; 
label values ws1_im13 ws1_im; 
label values ws1_im14 ws1_im; 
label values ws1_im15 ws1_im; 
label values ws1_im16 ws1_im; 
label values ws1_im17 ws1_im; 
label values ws1_im18 ws1_im; 
label values ws1_im19 ws1_im; 
label values ws1_im20 ws1_im; 
label values ws1_im21 ws1_im; 
label values ws1_im22 ws1_im; 
label values ws1_im23 ws1_im; 
label values ws1_im24 ws1_im; 
label values ws1_im25 ws1_im; 
label values ws1_im26 ws1_im; 
label values ws1_im27 ws1_im; 
label values ws1_im28 ws1_im; 
label values ws1_im29 ws1_im; 
label values ws1_im30 ws1_im; 
label values ws1_im31 ws1_im; 
label values ws1_im32 ws1_im; 
label define ws1_im  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values ws2_im01 ws2_im; 
label values ws2_im02 ws2_im; 
label values ws2_im03 ws2_im; 
label values ws2_im04 ws2_im; 
label values ws2_im05 ws2_im; 
label values ws2_im06 ws2_im; 
label values ws2_im07 ws2_im; 
label values ws2_im08 ws2_im; 
label values ws2_im09 ws2_im; 
label values ws2_im10 ws2_im; 
label values ws2_im11 ws2_im; 
label values ws2_im12 ws2_im; 
label values ws2_im13 ws2_im; 
label values ws2_im14 ws2_im; 
label values ws2_im15 ws2_im; 
label values ws2_im16 ws2_im; 
label values ws2_im17 ws2_im; 
label values ws2_im18 ws2_im; 
label values ws2_im19 ws2_im; 
label values ws2_im20 ws2_im; 
label values ws2_im21 ws2_im; 
label values ws2_im22 ws2_im; 
label values ws2_im23 ws2_im; 
label values ws2_im24 ws2_im; 
label values ws2_im25 ws2_im; 
label values ws2_im26 ws2_im; 
label values ws2_im27 ws2_im; 
label values ws2_im28 ws2_im; 
label values ws2_im29 ws2_im; 
label values ws2_im30 ws2_im; 
label values ws2_im31 ws2_im; 
label values ws2_im32 ws2_im; 
label define ws2_im  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values se1_im01 se1_im; 
label values se1_im02 se1_im; 
label values se1_im03 se1_im; 
label values se1_im04 se1_im; 
label values se1_im05 se1_im; 
label values se1_im06 se1_im; 
label values se1_im07 se1_im; 
label values se1_im08 se1_im; 
label values se1_im09 se1_im; 
label values se1_im10 se1_im; 
label values se1_im11 se1_im; 
label values se1_im12 se1_im; 
label values se1_im13 se1_im; 
label values se1_im14 se1_im; 
label values se1_im15 se1_im; 
label values se1_im16 se1_im; 
label values se1_im17 se1_im; 
label values se1_im18 se1_im; 
label values se1_im19 se1_im; 
label values se1_im20 se1_im; 
label values se1_im21 se1_im; 
label values se1_im22 se1_im; 
label values se1_im23 se1_im; 
label values se1_im24 se1_im; 
label values se1_im25 se1_im; 
label values se1_im26 se1_im; 
label values se1_im27 se1_im; 
label values se1_im28 se1_im; 
label values se1_im29 se1_im; 
label values se1_im30 se1_im; 
label values se1_im31 se1_im; 
label values se1_im32 se1_im; 
label define se1_im  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values se2_im01 se2_im; 
label values se2_im02 se2_im; 
label values se2_im03 se2_im; 
label values se2_im04 se2_im; 
label values se2_im05 se2_im; 
label values se2_im06 se2_im; 
label values se2_im07 se2_im; 
label values se2_im08 se2_im; 
label values se2_im09 se2_im; 
label values se2_im10 se2_im; 
label values se2_im11 se2_im; 
label values se2_im12 se2_im; 
label values se2_im13 se2_im; 
label values se2_im14 se2_im; 
label values se2_im15 se2_im; 
label values se2_im16 se2_im; 
label values se2_im17 se2_im; 
label values se2_im18 se2_im; 
label values se2_im19 se2_im; 
label values se2_im20 se2_im; 
label values se2_im21 se2_im; 
label values se2_im22 se2_im; 
label values se2_im23 se2_im; 
label values se2_im24 se2_im; 
label values se2_im25 se2_im; 
label values se2_im26 se2_im; 
label values se2_im27 se2_im; 
label values se2_im28 se2_im; 
label values se2_im29 se2_im; 
label values se2_im30 se2_im; 
label values se2_im31 se2_im; 
label values se2_im32 se2_im; 
label define se2_im  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i1_01 g1_i1l; 
label values g1_i1_02 g1_i1l; 
label values g1_i1_03 g1_i1l; 
label values g1_i1_04 g1_i1l; 
label values g1_i1_05 g1_i1l; 
label values g1_i1_06 g1_i1l; 
label values g1_i1_07 g1_i1l; 
label values g1_i1_08 g1_i1l; 
label values g1_i1_09 g1_i1l; 
label values g1_i1_10 g1_i1l; 
label values g1_i1_11 g1_i1l; 
label values g1_i1_12 g1_i1l; 
label values g1_i1_13 g1_i1l; 
label values g1_i1_14 g1_i1l; 
label values g1_i1_15 g1_i1l; 
label values g1_i1_16 g1_i1l; 
label values g1_i1_17 g1_i1l; 
label values g1_i1_18 g1_i1l; 
label values g1_i1_19 g1_i1l; 
label values g1_i1_20 g1_i1l; 
label values g1_i1_21 g1_i1l; 
label values g1_i1_22 g1_i1l; 
label values g1_i1_23 g1_i1l; 
label values g1_i1_24 g1_i1l; 
label values g1_i1_25 g1_i1l; 
label values g1_i1_26 g1_i1l; 
label values g1_i1_27 g1_i1l; 
label values g1_i1_28 g1_i1l; 
label values g1_i1_29 g1_i1l; 
label values g1_i1_30 g1_i1l; 
label values g1_i1_31 g1_i1l; 
label values g1_i1_32 g1_i1l; 
label define g1_i1l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i2_01 g1_i2l; 
label values g1_i2_02 g1_i2l; 
label values g1_i2_03 g1_i2l; 
label values g1_i2_04 g1_i2l; 
label values g1_i2_05 g1_i2l; 
label values g1_i2_06 g1_i2l; 
label values g1_i2_07 g1_i2l; 
label values g1_i2_08 g1_i2l; 
label values g1_i2_09 g1_i2l; 
label values g1_i2_10 g1_i2l; 
label values g1_i2_11 g1_i2l; 
label values g1_i2_12 g1_i2l; 
label values g1_i2_13 g1_i2l; 
label values g1_i2_14 g1_i2l; 
label values g1_i2_15 g1_i2l; 
label values g1_i2_16 g1_i2l; 
label values g1_i2_17 g1_i2l; 
label values g1_i2_18 g1_i2l; 
label values g1_i2_19 g1_i2l; 
label values g1_i2_20 g1_i2l; 
label values g1_i2_21 g1_i2l; 
label values g1_i2_22 g1_i2l; 
label values g1_i2_23 g1_i2l; 
label values g1_i2_24 g1_i2l; 
label values g1_i2_25 g1_i2l; 
label values g1_i2_26 g1_i2l; 
label values g1_i2_27 g1_i2l; 
label values g1_i2_28 g1_i2l; 
label values g1_i2_29 g1_i2l; 
label values g1_i2_30 g1_i2l; 
label values g1_i2_31 g1_i2l; 
label values g1_i2_32 g1_i2l; 
label define g1_i2l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i3_01 g1_i3l; 
label values g1_i3_02 g1_i3l; 
label values g1_i3_03 g1_i3l; 
label values g1_i3_04 g1_i3l; 
label values g1_i3_05 g1_i3l; 
label values g1_i3_06 g1_i3l; 
label values g1_i3_07 g1_i3l; 
label values g1_i3_08 g1_i3l; 
label values g1_i3_09 g1_i3l; 
label values g1_i3_10 g1_i3l; 
label values g1_i3_11 g1_i3l; 
label values g1_i3_12 g1_i3l; 
label values g1_i3_13 g1_i3l; 
label values g1_i3_14 g1_i3l; 
label values g1_i3_15 g1_i3l; 
label values g1_i3_16 g1_i3l; 
label values g1_i3_17 g1_i3l; 
label values g1_i3_18 g1_i3l; 
label values g1_i3_19 g1_i3l; 
label values g1_i3_20 g1_i3l; 
label values g1_i3_21 g1_i3l; 
label values g1_i3_22 g1_i3l; 
label values g1_i3_23 g1_i3l; 
label values g1_i3_24 g1_i3l; 
label values g1_i3_25 g1_i3l; 
label values g1_i3_26 g1_i3l; 
label values g1_i3_27 g1_i3l; 
label values g1_i3_28 g1_i3l; 
label values g1_i3_29 g1_i3l; 
label values g1_i3_30 g1_i3l; 
label values g1_i3_31 g1_i3l; 
label values g1_i3_32 g1_i3l; 
label define g1_i3l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i4_01 g1_i4l; 
label values g1_i4_02 g1_i4l; 
label values g1_i4_03 g1_i4l; 
label values g1_i4_04 g1_i4l; 
label values g1_i4_05 g1_i4l; 
label values g1_i4_06 g1_i4l; 
label values g1_i4_07 g1_i4l; 
label values g1_i4_08 g1_i4l; 
label values g1_i4_09 g1_i4l; 
label values g1_i4_10 g1_i4l; 
label values g1_i4_11 g1_i4l; 
label values g1_i4_12 g1_i4l; 
label values g1_i4_13 g1_i4l; 
label values g1_i4_14 g1_i4l; 
label values g1_i4_15 g1_i4l; 
label values g1_i4_16 g1_i4l; 
label values g1_i4_17 g1_i4l; 
label values g1_i4_18 g1_i4l; 
label values g1_i4_19 g1_i4l; 
label values g1_i4_20 g1_i4l; 
label values g1_i4_21 g1_i4l; 
label values g1_i4_22 g1_i4l; 
label values g1_i4_23 g1_i4l; 
label values g1_i4_24 g1_i4l; 
label values g1_i4_25 g1_i4l; 
label values g1_i4_26 g1_i4l; 
label values g1_i4_27 g1_i4l; 
label values g1_i4_28 g1_i4l; 
label values g1_i4_29 g1_i4l; 
label values g1_i4_30 g1_i4l; 
label values g1_i4_31 g1_i4l; 
label values g1_i4_32 g1_i4l; 
label define g1_i4l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i5_01 g1_i5l; 
label values g1_i5_02 g1_i5l; 
label values g1_i5_03 g1_i5l; 
label values g1_i5_04 g1_i5l; 
label values g1_i5_05 g1_i5l; 
label values g1_i5_06 g1_i5l; 
label values g1_i5_07 g1_i5l; 
label values g1_i5_08 g1_i5l; 
label values g1_i5_09 g1_i5l; 
label values g1_i5_10 g1_i5l; 
label values g1_i5_11 g1_i5l; 
label values g1_i5_12 g1_i5l; 
label values g1_i5_13 g1_i5l; 
label values g1_i5_14 g1_i5l; 
label values g1_i5_15 g1_i5l; 
label values g1_i5_16 g1_i5l; 
label values g1_i5_17 g1_i5l; 
label values g1_i5_18 g1_i5l; 
label values g1_i5_19 g1_i5l; 
label values g1_i5_20 g1_i5l; 
label values g1_i5_21 g1_i5l; 
label values g1_i5_22 g1_i5l; 
label values g1_i5_23 g1_i5l; 
label values g1_i5_24 g1_i5l; 
label values g1_i5_25 g1_i5l; 
label values g1_i5_26 g1_i5l; 
label values g1_i5_27 g1_i5l; 
label values g1_i5_28 g1_i5l; 
label values g1_i5_29 g1_i5l; 
label values g1_i5_30 g1_i5l; 
label values g1_i5_31 g1_i5l; 
label values g1_i5_32 g1_i5l; 
label define g1_i5l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i6_01 g1_i6l; 
label values g1_i6_02 g1_i6l; 
label values g1_i6_03 g1_i6l; 
label values g1_i6_04 g1_i6l; 
label values g1_i6_05 g1_i6l; 
label values g1_i6_06 g1_i6l; 
label values g1_i6_07 g1_i6l; 
label values g1_i6_08 g1_i6l; 
label values g1_i6_09 g1_i6l; 
label values g1_i6_10 g1_i6l; 
label values g1_i6_11 g1_i6l; 
label values g1_i6_12 g1_i6l; 
label values g1_i6_13 g1_i6l; 
label values g1_i6_14 g1_i6l; 
label values g1_i6_15 g1_i6l; 
label values g1_i6_16 g1_i6l; 
label values g1_i6_17 g1_i6l; 
label values g1_i6_18 g1_i6l; 
label values g1_i6_19 g1_i6l; 
label values g1_i6_20 g1_i6l; 
label values g1_i6_21 g1_i6l; 
label values g1_i6_22 g1_i6l; 
label values g1_i6_23 g1_i6l; 
label values g1_i6_24 g1_i6l; 
label values g1_i6_25 g1_i6l; 
label values g1_i6_26 g1_i6l; 
label values g1_i6_27 g1_i6l; 
label values g1_i6_28 g1_i6l; 
label values g1_i6_29 g1_i6l; 
label values g1_i6_30 g1_i6l; 
label values g1_i6_31 g1_i6l; 
label values g1_i6_32 g1_i6l; 
label define g1_i6l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i7_01 g1_i7l; 
label values g1_i7_02 g1_i7l; 
label values g1_i7_03 g1_i7l; 
label values g1_i7_04 g1_i7l; 
label values g1_i7_05 g1_i7l; 
label values g1_i7_06 g1_i7l; 
label values g1_i7_07 g1_i7l; 
label values g1_i7_08 g1_i7l; 
label values g1_i7_09 g1_i7l; 
label values g1_i7_10 g1_i7l; 
label values g1_i7_11 g1_i7l; 
label values g1_i7_12 g1_i7l; 
label values g1_i7_13 g1_i7l; 
label values g1_i7_14 g1_i7l; 
label values g1_i7_15 g1_i7l; 
label values g1_i7_16 g1_i7l; 
label values g1_i7_17 g1_i7l; 
label values g1_i7_18 g1_i7l; 
label values g1_i7_19 g1_i7l; 
label values g1_i7_20 g1_i7l; 
label values g1_i7_21 g1_i7l; 
label values g1_i7_22 g1_i7l; 
label values g1_i7_23 g1_i7l; 
label values g1_i7_24 g1_i7l; 
label values g1_i7_25 g1_i7l; 
label values g1_i7_26 g1_i7l; 
label values g1_i7_27 g1_i7l; 
label values g1_i7_28 g1_i7l; 
label values g1_i7_29 g1_i7l; 
label values g1_i7_30 g1_i7l; 
label values g1_i7_31 g1_i7l; 
label values g1_i7_32 g1_i7l; 
label define g1_i7l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i8_01 g1_i8l; 
label values g1_i8_02 g1_i8l; 
label values g1_i8_03 g1_i8l; 
label values g1_i8_04 g1_i8l; 
label values g1_i8_05 g1_i8l; 
label values g1_i8_06 g1_i8l; 
label values g1_i8_07 g1_i8l; 
label values g1_i8_08 g1_i8l; 
label values g1_i8_09 g1_i8l; 
label values g1_i8_10 g1_i8l; 
label values g1_i8_11 g1_i8l; 
label values g1_i8_12 g1_i8l; 
label values g1_i8_13 g1_i8l; 
label values g1_i8_14 g1_i8l; 
label values g1_i8_15 g1_i8l; 
label values g1_i8_16 g1_i8l; 
label values g1_i8_17 g1_i8l; 
label values g1_i8_18 g1_i8l; 
label values g1_i8_19 g1_i8l; 
label values g1_i8_20 g1_i8l; 
label values g1_i8_21 g1_i8l; 
label values g1_i8_22 g1_i8l; 
label values g1_i8_23 g1_i8l; 
label values g1_i8_24 g1_i8l; 
label values g1_i8_25 g1_i8l; 
label values g1_i8_26 g1_i8l; 
label values g1_i8_27 g1_i8l; 
label values g1_i8_28 g1_i8l; 
label values g1_i8_29 g1_i8l; 
label values g1_i8_30 g1_i8l; 
label values g1_i8_31 g1_i8l; 
label values g1_i8_32 g1_i8l; 
label define g1_i8l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i9_01 g1_i9l; 
label values g1_i9_02 g1_i9l; 
label values g1_i9_03 g1_i9l; 
label values g1_i9_04 g1_i9l; 
label values g1_i9_05 g1_i9l; 
label values g1_i9_06 g1_i9l; 
label values g1_i9_07 g1_i9l; 
label values g1_i9_08 g1_i9l; 
label values g1_i9_09 g1_i9l; 
label values g1_i9_10 g1_i9l; 
label values g1_i9_11 g1_i9l; 
label values g1_i9_12 g1_i9l; 
label values g1_i9_13 g1_i9l; 
label values g1_i9_14 g1_i9l; 
label values g1_i9_15 g1_i9l; 
label values g1_i9_16 g1_i9l; 
label values g1_i9_17 g1_i9l; 
label values g1_i9_18 g1_i9l; 
label values g1_i9_19 g1_i9l; 
label values g1_i9_20 g1_i9l; 
label values g1_i9_21 g1_i9l; 
label values g1_i9_22 g1_i9l; 
label values g1_i9_23 g1_i9l; 
label values g1_i9_24 g1_i9l; 
label values g1_i9_25 g1_i9l; 
label values g1_i9_26 g1_i9l; 
label values g1_i9_27 g1_i9l; 
label values g1_i9_28 g1_i9l; 
label values g1_i9_29 g1_i9l; 
label values g1_i9_30 g1_i9l; 
label values g1_i9_31 g1_i9l; 
label values g1_i9_32 g1_i9l; 
label define g1_i9l  
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g1_i1001 g1_i10l;
label values g1_i1002 g1_i10l;
label values g1_i1003 g1_i10l;
label values g1_i1004 g1_i10l;
label values g1_i1005 g1_i10l;
label values g1_i1006 g1_i10l;
label values g1_i1007 g1_i10l;
label values g1_i1008 g1_i10l;
label values g1_i1009 g1_i10l;
label values g1_i1010 g1_i10l;
label values g1_i1011 g1_i10l;
label values g1_i1012 g1_i10l;
label values g1_i1013 g1_i10l;
label values g1_i1014 g1_i10l;
label values g1_i1015 g1_i10l;
label values g1_i1016 g1_i10l;
label values g1_i1017 g1_i10l;
label values g1_i1018 g1_i10l;
label values g1_i1019 g1_i10l;
label values g1_i1020 g1_i10l;
label values g1_i1021 g1_i10l;
label values g1_i1022 g1_i10l;
label values g1_i1023 g1_i10l;
label values g1_i1024 g1_i10l;
label values g1_i1025 g1_i10l;
label values g1_i1026 g1_i10l;
label values g1_i1027 g1_i10l;
label values g1_i1028 g1_i10l;
label values g1_i1029 g1_i10l;
label values g1_i1030 g1_i10l;
label values g1_i1031 g1_i10l;
label values g1_i1032 g1_i10l;
label define g1_i10l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i10001 g2i100l;
label values g2i10002 g2i100l;
label values g2i10003 g2i100l;
label values g2i10004 g2i100l;
label values g2i10005 g2i100l;
label values g2i10006 g2i100l;
label values g2i10007 g2i100l;
label values g2i10008 g2i100l;
label values g2i10009 g2i100l;
label values g2i10010 g2i100l;
label values g2i10011 g2i100l;
label values g2i10012 g2i100l;
label values g2i10013 g2i100l;
label values g2i10014 g2i100l;
label values g2i10015 g2i100l;
label values g2i10016 g2i100l;
label values g2i10017 g2i100l;
label values g2i10018 g2i100l;
label values g2i10019 g2i100l;
label values g2i10020 g2i100l;
label values g2i10021 g2i100l;
label values g2i10022 g2i100l;
label values g2i10023 g2i100l;
label values g2i10024 g2i100l;
label values g2i10025 g2i100l;
label values g2i10026 g2i100l;
label values g2i10027 g2i100l;
label values g2i10028 g2i100l;
label values g2i10029 g2i100l;
label values g2i10030 g2i100l;
label values g2i10031 g2i100l;
label values g2i10032 g2i100l;
label define g2i100l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i10401 g2i104l;
label values g2i10402 g2i104l;
label values g2i10403 g2i104l;
label values g2i10404 g2i104l;
label values g2i10405 g2i104l;
label values g2i10406 g2i104l;
label values g2i10407 g2i104l;
label values g2i10408 g2i104l;
label values g2i10409 g2i104l;
label values g2i10410 g2i104l;
label values g2i10411 g2i104l;
label values g2i10412 g2i104l;
label values g2i10413 g2i104l;
label values g2i10414 g2i104l;
label values g2i10415 g2i104l;
label values g2i10416 g2i104l;
label values g2i10417 g2i104l;
label values g2i10418 g2i104l;
label values g2i10419 g2i104l;
label values g2i10420 g2i104l;
label values g2i10421 g2i104l;
label values g2i10422 g2i104l;
label values g2i10423 g2i104l;
label values g2i10424 g2i104l;
label values g2i10425 g2i104l;
label values g2i10426 g2i104l;
label values g2i10427 g2i104l;
label values g2i10428 g2i104l;
label values g2i10429 g2i104l;
label values g2i10430 g2i104l;
label values g2i10431 g2i104l;
label values g2i10432 g2i104l;
label define g2i104l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i11001 g2i110l;
label values g2i11002 g2i110l;
label values g2i11003 g2i110l;
label values g2i11004 g2i110l;
label values g2i11005 g2i110l;
label values g2i11006 g2i110l;
label values g2i11007 g2i110l;
label values g2i11008 g2i110l;
label values g2i11009 g2i110l;
label values g2i11010 g2i110l;
label values g2i11011 g2i110l;
label values g2i11012 g2i110l;
label values g2i11013 g2i110l;
label values g2i11014 g2i110l;
label values g2i11015 g2i110l;
label values g2i11016 g2i110l;
label values g2i11017 g2i110l;
label values g2i11018 g2i110l;
label values g2i11019 g2i110l;
label values g2i11020 g2i110l;
label values g2i11021 g2i110l;
label values g2i11022 g2i110l;
label values g2i11023 g2i110l;
label values g2i11024 g2i110l;
label values g2i11025 g2i110l;
label values g2i11026 g2i110l;
label values g2i11027 g2i110l;
label values g2i11028 g2i110l;
label values g2i11029 g2i110l;
label values g2i11030 g2i110l;
label values g2i11031 g2i110l;
label values g2i11032 g2i110l;
label define g2i110l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i12001 g2i120l;
label values g2i12002 g2i120l;
label values g2i12003 g2i120l;
label values g2i12004 g2i120l;
label values g2i12005 g2i120l;
label values g2i12006 g2i120l;
label values g2i12007 g2i120l;
label values g2i12008 g2i120l;
label values g2i12009 g2i120l;
label values g2i12010 g2i120l;
label values g2i12011 g2i120l;
label values g2i12012 g2i120l;
label values g2i12013 g2i120l;
label values g2i12014 g2i120l;
label values g2i12015 g2i120l;
label values g2i12016 g2i120l;
label values g2i12017 g2i120l;
label values g2i12018 g2i120l;
label values g2i12019 g2i120l;
label values g2i12020 g2i120l;
label values g2i12021 g2i120l;
label values g2i12022 g2i120l;
label values g2i12023 g2i120l;
label values g2i12024 g2i120l;
label values g2i12025 g2i120l;
label values g2i12026 g2i120l;
label values g2i12027 g2i120l;
label values g2i12028 g2i120l;
label values g2i12029 g2i120l;
label values g2i12030 g2i120l;
label values g2i12031 g2i120l;
label values g2i12032 g2i120l;
label define g2i120l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i13001 g2i130l;
label values g2i13002 g2i130l;
label values g2i13003 g2i130l;
label values g2i13004 g2i130l;
label values g2i13005 g2i130l;
label values g2i13006 g2i130l;
label values g2i13007 g2i130l;
label values g2i13008 g2i130l;
label values g2i13009 g2i130l;
label values g2i13010 g2i130l;
label values g2i13011 g2i130l;
label values g2i13012 g2i130l;
label values g2i13013 g2i130l;
label values g2i13014 g2i130l;
label values g2i13015 g2i130l;
label values g2i13016 g2i130l;
label values g2i13017 g2i130l;
label values g2i13018 g2i130l;
label values g2i13019 g2i130l;
label values g2i13020 g2i130l;
label values g2i13021 g2i130l;
label values g2i13022 g2i130l;
label values g2i13023 g2i130l;
label values g2i13024 g2i130l;
label values g2i13025 g2i130l;
label values g2i13026 g2i130l;
label values g2i13027 g2i130l;
label values g2i13028 g2i130l;
label values g2i13029 g2i130l;
label values g2i13030 g2i130l;
label values g2i13031 g2i130l;
label values g2i13032 g2i130l;
label define g2i130l 
	0           "NOT IMPUTED"                   
	1           "IMPUTED"                       
;
label values g2i14001 g2i140l;
label values g2i14002 g2i140l;
label values g2i14003 g2i140l;
label values g2i14004 g2i140l;
label values g2i14005 g2i140l;
label values g2i14006 g2i140l;
label values g2i14007 g2i140l;
label values g2i14008 g2i140l;
label values g2i14009 g2i140l;
label values g2i14010 g2i140l;
label values g2i14011 g2i140l;
label values g2i14012 g2i140l;
label values g2i14013 g2i140l;
label values g2i14014 g2i140l;
label values g2i14015 g2i140l;
label values g2i14016 g2i140l;
label values g2i14017 g2i140l;
label values g2i14018 g2i140l;
label values g2i14019 g2i140l;
label values g2i14020 g2i140l;
label values g2i14021 g2i140l;
label values g2i14022 g2i140l;
label values g2i14023 g2i140l;
label values g2i14024 g2i140l;
label values g2i14025 g2i140l;
label values g2i14026 g2i140l;
label values g2i14027 g2i140l;
label values g2i14028 g2i140l;
label values g2i14029 g2i140l;
label values g2i14030 g2i140l;
label values g2i14031 g2i140l;
label values g2i14032 g2i140l;
label define g2i140l 
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
