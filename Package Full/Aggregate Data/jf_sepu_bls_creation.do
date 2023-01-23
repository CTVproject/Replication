
/* jf_sepu_bls.dta was created by copy-pasting 4 series into the data editor:
LNS17100000
LNS12000000 
LNS17400000
LNS13000000
These series were accessed in August 2018. 

Job finding series constructed as LNS17100000/LNS12000000, which here are renames as  UE_numbers_bls/ unempl_level_bls
Separation Rate series constructed as LNS17400000/LNS13000000, which here are renames as   EU_numbers_bls/ empl_level_bls

These 4 LNS-series can be accessed via https://data.bls.gov/cgi-bin/srgate . We use the data here as accessed at the end
of 2018. 

These series are included here mainly to allow for robustness testing; the only series that uses jf_bls as an ingredient
is the job finding time series for workers in NUN spells in the Online Appendix (where addressing censoring issues affects the number of quarters
that can be used/have a large enough number of observations; we use the BLS jf rate to instrument this series).
*/
version 13
	
do "${workingdir}/global_paths.do"



gen jf_bls= UE_numbers_bls/ unempl_level_bls
gen sepu_bls= EU_numbers_bls/ empl_level_bls
notes jf_bls=LNS17100000/LNS12000000
notes sepu_bls=LNS17400000/LNS13000000

format quarter %tq

sort quarter
keep jf_bls sepu_bls quarter
duplicates drop
drop if jf_bls==. & sepu_bls==.
save "${workingdir}/Aggregate Data/jf_sepu_bls.dta"
