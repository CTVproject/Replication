
********************************************************************************
*
* Copyright 2023 Ludo Visschers and Carlos Carrillo-Tudela
*
*
* Redistribution and use in source and binary forms, with or without 
*	modification, are permitted provided that the following conditions are met:
*
* 1. Redistributions of source code must retain the above copyright notice, 
*	this list of conditions and the following disclaimer.
*
* 2. Redistributions in binary form must reproduce the above copyright notice, 
*	this list of conditions and the following disclaimer in the documentation 
*	and/or other materials provided with the distribution.
*
* 3. Neither the name of the copyright holder nor the names of its contributors 
*	may be used to endorse or promote products derived from this software 
*	without specific prior written permission.
*
* 4. If using this code or its results (whole or in part, with or without 
* 	modifications) in academic work, please cite:  
*		Carrillo-Tudela, Carlos and Ludo Visschers, "Unemployment and Endogenous 
*		Reallocation over the Business Cycle" 
*	in its published version, in the references of the publications associated 
*	with aforementioned academic work.
*
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*

********************************************************************************







********************************************************************************
***
*** 1. HEATMAPS OF GROSS/NET MOBILITY VERSION PER SUPEROCC
*** 2. PRODUCTIVITY AND REL. IMPORTANCE SEARCH/REST IN GROSS/NET MOB, P. SUPEROCC
***
********************************************************************************



	clear
	do "${workingdir}/global_paths.do"
	
	version 13
	
	set more off
	set varabbrev off

	
	global lstarttime=c(current_time)
	global lstartdate=c(current_date)
	display "started at ${lstarttime} on ${lstartdate}"











//==============================================================================
// SUPER OCCUPATION SPECIFIC -- FIGURE 8 MAIN TEXT
//==============================================================================


import delimited "${fortrandir}/Gross and Net Mobility version/dist_bests10_000.csv", clear 
global lversion "netmob"
global netmob1_xsmob0=1


cap n ssc install heatplot, replace
cap n ssc install palettes, replace
cap n ssc install colrspace, replace

colorpalette matplotlib hot, reverse
colorpalette sfso olive

#delimit cr
capture drop eodensity
/*gen eodensity=eopz_dist*/
bysort oidx: egen eodensity=sum(eopz_dist) if p_level>0.94 & p_level<1.06

capture drop uodensity
/*bysort oidx: */
bysort oidx: egen uodensity=sum(uopz_dist) if p_level>0.94 & p_level<1.06
replace uodensity=uopz_dist/(uodensity+eodensity)
replace uodensity=. if uodensity<0.0000005

*normalize eodensity
*bysort oidx: egen eodensity=sum(eopz_dist) if p_level>0.94 & p_level<1.06
replace eodensity=eopz_dist/(eodensity)
replace eodensity=. if eodensity<0.0000005

// COLORED VERSIONS
//XXXX
global topcut=0.0001
global bottomcut=-0.0000005
global cutsize=0.0000005
global ytop=1.02
global ybottom=0.4

sort oidx xidx pidx zidx
#delimit ;
heatplot uodensity z_level p_level if oidx==1 & p_level>0.94 & p_level<1.06,color(Greens) cuts($bottomcut($cutsize)$topcut) discrete($discrete_factor2) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
ysc(r($ybottom $ytop)) ylabel($ybottom(0.2)$ytop)
/*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_s_val==z_level & xidx==1 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_val==z_level & xidx==2 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash) 
|| scatter z_level p_level if z_s_val==z_level & xidx==3 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick)  lpattern(longdash)
|| scatter z_level p_level if z_r_val==z_level & xidx==1 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==2 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==3 & oidx==1 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
/*legend(order(1 2 3 4) label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM"))*/
graphregion(color(white)) ylabel(0.4(0.2)1.0, labsize(large)) 
/*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
); 
graph export "${mainresultsdir}/fig8b.pdf", as(pdf) replace;


#delimit ;
heatplot uodensity z_level p_level if oidx==2 & p_level>0.94 & p_level<1.06,color(sfso olive, reverse) cuts($bottomcut($cutsize)$topcut) discrete($discrete_factor2) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
ysc(r($ybottom $ytop)) ylabel($ybottom(0.2)$ytop)
/*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_s_val==z_level & xidx==1 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_val==z_level & xidx==2 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash) 
|| scatter z_level p_level if z_s_val==z_level & xidx==3 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick)  lpattern(longdash)
|| scatter z_level p_level if z_r_val==z_level & xidx==1 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==2 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==3 & oidx==2 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
/*legend(order(1 2 3 4) label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM"))*/
graphregion(color(white)) ylabel(0.4(0.2)1.0, labsize(large)) 
/*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
); 
graph export "${mainresultsdir}/fig8c.pdf", as(pdf) replace;


#delimit ;
heatplot uodensity z_level p_level if oidx==3 & p_level>0.94 & p_level<1.06,color(Reds) cuts($bottomcut($cutsize)$topcut) discrete($discrete_factor2) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
ysc(r($ybottom $ytop)) ylabel($ybottom(0.2)$ytop)
/*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_s_val==z_level & xidx==1 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_val==z_level & xidx==2 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash) 
|| scatter z_level p_level if z_s_val==z_level & xidx==3 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick)  lpattern(longdash)
|| scatter z_level p_level if z_r_val==z_level & xidx==1 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==2 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==3 & oidx==3 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
/*legend(order(1 2 3 4) label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM"))*/
graphregion(color(white)) ylabel(0.4(0.2)1.0, labsize(large)) 
/*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
); 
graph export "${mainresultsdir}/fig8e.pdf", as(pdf) replace;



#delimit ;
heatplot uodensity z_level p_level if oidx==4 & p_level>0.94 & p_level<1.06,color(Blues) cuts($bottomcut($cutsize)$topcut) discrete($discrete_factor2) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
ysc(r($ybottom $ytop)) ylabel($ybottom(0.2)$ytop)
/*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_s_val==z_level & xidx==1 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_val==z_level & xidx==2 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash) 
|| scatter z_level p_level if z_s_val==z_level & xidx==3 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick)  lpattern(longdash)
|| scatter z_level p_level if z_r_val==z_level & xidx==1 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==2 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_r_val==z_level & xidx==3 & oidx==4 & p_level>0.94 & p_level<1.06, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) 
/*legend(order(1 2 3 4) label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM"))*/
graphregion(color(white)) ylabel(0.4(0.2)1.0, labsize(large)) 
/*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
); 
graph export "${mainresultsdir}/fig8f.pdf", as(pdf) replace;
#delimit cr



//=========================================
// DECOMPOSITION
//=========================================



capture drop up_search_prop
capture drop up_reall_prop
capture drop up_rest_prop

capture drop up_search_yng_prop
capture drop up_reall_yng_prop
capture drop up_rest_yng_prop

capture drop up_search_prm_prop
capture drop up_reall_prm_prop
capture drop up_rest_prm_prop



capture drop uop_search_prop
capture drop uop_reall_prop
capture drop uop_rest_prop

capture drop uop_search_yng_prop
capture drop uop_reall_yng_prop
capture drop uop_rest_yng_prop

capture drop uop_search_prm_prop
capture drop uop_reall_prm_prop
capture drop uop_rest_prm_prop



gen up_search_prop=up_search_dist/(up_search_dist+up_reall_dist+up_rest_dist)
gen up_reall_prop=up_reall_dist/(up_search_dist+up_reall_dist+up_rest_dist)
gen up_rest_prop=up_rest_dist/(up_search_dist+up_reall_dist+up_rest_dist)

gen up_search_yng_prop=up_search_dist_young/(up_search_dist_young+up_reall_dist_young+up_rest_dist_young)
gen up_reall_yng_prop=up_reall_dist_young/(up_search_dist_young+up_reall_dist_young+up_rest_dist_young)
gen up_rest_yng_prop=up_rest_dist_young/(up_search_dist_young+up_reall_dist_young+up_rest_dist_young)


gen up_search_prm_prop=up_search_dist_young/(up_search_dist_prime+up_reall_dist_prime+up_rest_dist_prime)
gen up_reall_prm_prop=up_reall_dist_young/(up_search_dist_prime+up_reall_dist_prime+up_rest_dist_prime)
gen up_rest_prm_prop=up_rest_dist_young/(up_search_dist_prime+up_reall_dist_prime+up_rest_dist_prime)



gen uop_search_prop=uop_search_dist/(uop_search_dist+uop_reall_dist+uop_rest_dist)
gen uop_reall_prop=uop_reall_dist/(uop_search_dist+uop_reall_dist+uop_rest_dist)
gen uop_rest_prop=uop_rest_dist/(uop_search_dist+uop_reall_dist+uop_rest_dist)

gen uop_search_yng_prop=uop_search_dist_young/(uop_search_dist_young+uop_reall_dist_young+uop_rest_dist_young)
gen uop_reall_yng_prop=uop_reall_dist_young/(uop_search_dist_young+uop_reall_dist_young+uop_rest_dist_young)
gen uop_rest_yng_prop=uop_rest_dist_young/(uop_search_dist_young+uop_reall_dist_young+uop_rest_dist_young)


gen uop_search_prm_prop=uop_search_dist_young/(uop_search_dist_prime+uop_reall_dist_prime+uop_rest_dist_prime)
gen uop_reall_prm_prop=uop_reall_dist_young/(uop_search_dist_prime+uop_reall_dist_prime+uop_rest_dist_prime)
gen uop_rest_prm_prop=uop_rest_dist_young/(uop_search_dist_prime+uop_reall_dist_prime+uop_rest_dist_prime)


capture drop uop_searchrestrel_prop
capture drop uop_searchrestrel_yng_prop
capture drop uop_searchrestrel_prm_prop

capture drop up_searchrestrel_prop
capture drop up_searchrestrel_yng_prop
capture drop up_searchrestrel_prm_prop


gen uop_searchrestrel_prop=uop_rest_prm_prop/uop_search_prm_prop
gen up_searchrestrel_prop=up_rest_prm_prop/up_search_prm_prop



//XXXXXXXXXXXXXXxx
	sort oidx pidx xidx zidx 
	#delimit ;
	scatter uop_searchrestrel_prop p_level if (oidx==1 ) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(green	) mcolor(green) msymbol(D ) lwidth(thick) lpattern(solid)
	xsize(4) ysize(3)
	||
	scatter uop_searchrestrel_prop p_level if (oidx==2) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(sand) mcolor(sand) msymbol(Oh) msize(large) mlwidth(medthick) lwidth(thick) lpattern(dash)
	||
	scatter uop_searchrestrel_prop p_level if (oidx==3) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(red) mcolor(red) msymbol(T ) lwidth(thick) lpattern(longdash)
	|| 
	scatter uop_searchrestrel_prop p_level if (oidx==4) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(edkblue) mcolor(edkblue) msymbol(Sh) msize(large) mlwidth(medthick) lwidth(thick) lpattern(_.)
	||
	scatter up_searchrestrel_prop p_level if (oidx==1) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(black%20) mcolor(gs8) msymbol(+) msize(large) graphregion(color(white)) lwidth(thick)
	legend(label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM") label(5 "All") ring(0) pos(1) size(*1.3)) ytitle("Ratio Rest/Search Unemp.",size(vlarge)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ylabel(0(0.5)2, labsize(large)) 
	;
	graph export "${mainresultsdir}/fig8d.pdf", as(pdf) replace;
#delimit cr
//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




** average occ productivity in the economy
capture drop eop_dist
bysort pidx oidx: egen eop_dist=sum(eopz_dist) if xidx==1 

sort pidx oidx xidx zidx
capture drop aveprod_p*
gen aveprod_p1=eop_dist*prod if oidx==1
gen aveprod_p2=eop_dist*prod if oidx==2
gen aveprod_p3=eop_dist*prod if oidx==3
gen aveprod_p4=eop_dist*prod if oidx==4

*replace 


//XXXXXXXXXXXXXXX
sort pidx oidx xidx zidx

#delimit ;
scatter prod p_level if oidx==1 & p_level>0.94 & p_level<1.06, connect(l) lcolor(green) lpattern(solid) mcolor(green)  msymbol(D) lwidth(thick)
|| 
scatter prod p_level if oidx==2 & p_level>0.94 & p_level<1.06, connect(l) lcolor(sand) lpattern(dash) mcolor(sand) msymbol(Oh) msize(large) mlwidth(medthick) lwidth(thick) 
|| 
scatter prod p_level if oidx==3 & p_level>0.94 & p_level<1.06, connect(l) lcolor(red) lpattern(longdash) mcolor(red) msymbol(T) lwidth(thick) 
|| 
scatter prod p_level if oidx==4 & p_level>0.94 & p_level<1.06, connect(l) lcolor(edkblue) lpattern(_.) mcolor(edkblue) msymbol(Sh) msize(large) mlwidth(medthick)  lwidth(thick)
/*|| 
*scatter p_level p_level if oidx==1, connect(l) lcolor(black%20) lpattern(solid) msymbol(i) lwidth(medthin)*/
||
scatter p_level p_level if (oidx==1) & p_level>0.94 & p_level<1.06, connect(l l) lcolor(black%20) mcolor(gs8) msymbol(+) msize(large) graphregion(color(white)) lwidth(thick)
legend(order(1 2 3 4 5) label(1 "NRC") label(2 "RC") label(3 "NRM") label(4 "RM") label(5 "Average") ring(0) pos(11) size(*1.3))
graphregion(color(white)) ysize(3) xsize(4) xtitle("Aggregate Productivity Level ({it:A})", size(vlarge))
ytitle("Occupation-wide Prod. Level {it:Ap{sub:o}}", size(large)) xlabel(0.94(0.02)1.06, labsize(large)) ylabel(0.9(0.05)1.1, labsize(large)) 
; 
#delimit cr
graph export "${mainresultsdir}/fig8a.pdf", as(pdf) replace
//XXXXXXXXX


global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"
