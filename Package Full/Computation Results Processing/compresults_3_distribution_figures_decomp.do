
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
*** 1. HEATMAPS OF DISTRIBUTION OVER Z, WITH CYCLE A, FIGS
*** 2. UNEMPLOYMENT DECOMPOSITION IN REST, SEARCH AND REALLOCATION U, FIGS
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




cap n ssc install heatplot, replace
cap n ssc install palettes, replace
cap n ssc install colrspace, replace

colorpalette matplotlib hot, reverse
colorpalette sfso olive


********************************************************************************

*** EXCESS MOBILITY BASELINE

********************************************************************************


import delimited "${fortrandir}/Excess Mobility version/dist_bests7_000.csv", numericcols(66) clear 
global lversion xsmob



global discrete_factor=0.008
global discrete_factor2=0.012

cap gen z_r_binary=1 if z_r_val==z_level
cap gen z_s_binary=1 if z_s_val==z_level
cap gen x_index=xidx

#delimit ;
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) lpattern(dash_dot)
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(gs8) lwidth(medthick) lpattern(dash_dot)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(gs8) lwidth(medthick) lpattern(longdash)
||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(g) lwidth(medthick)
xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) legend(label(1 "zr1") label(2 "zs1") label(3 "zr2") label(4 "zs2") label(5 "zr3") label(6 "zs3"))
graphregion(color(white)) /*name(cutoffs, replace)*/ saving(cutoffs.gph, replace)
;
#delimit cr


heatplot upz_dist z_level p_level,color(matplotlib hot, reverse) cuts(0.0002(0.00005)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) ramp
 
 
/*OrRd*/
capture drop zslabel
gen str zslabel= ""
replace zslabel= "z{subscript:s}(A, x{subscript:1}) " if x_index==1
replace zslabel= "z{subscript:s}(A, x{subscript:2}) " if x_index==2
replace zslabel= "z{subscript:s}(A, x{subscript:3}) " if x_index==3
capture drop zrlabel
gen str zrlabel= ""
replace zrlabel= "z{subscript:r}(A, x{subscript:1}) " if x_index==1
replace zrlabel= "z{subscript:r}(A, x{subscript:2}) " if x_index==2
replace zrlabel= "z{subscript:r}(A, x{subscript:3}) " if x_index==3


lab var upz_dist "u density"

capture drop udensity
gen udensity=upz_dist
histogram udensity

replace udensity=. if udensity<0.00003

#delimit ;
/*heatplot upz_dist z_level p_level,color(hcl, heat reverse) cuts(0.0002(0.0001)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) addplot(*/
heatplot udensity z_level p_level,color(matplotlib hot, reverse) cuts(-0.0001(0.00005)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(1) mlabgap(0)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) ylabel(0.4(0.2)1.0, labsize(large))
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
;
#delimit cr
graph export "${mainresultsdir}/fig7a.pdf", as(pdf) replace



//=================================================
// YOUNG AND PRIME AGE WITH SAME SCALE AS BEFORE 
//=================================================


capture drop udensity_yng
gen udensity_yng=upz_dist_young
histogram udensity_yng

replace udensity_yng=. if udensity_yng<0.00003


#delimit ;
/*heatplot upz_dist z_level p_level,color(hcl, heat reverse) cuts(0.0002(0.0001)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) addplot(*/
heatplot udensity_yng z_level p_level,color(matplotlib hot, reverse) cuts(-0.0002(0.0001)0.010) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(1) mlabgap(0)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) ylabel(0.4(0.2)1.0, labsize(large))
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)
;
#delimit cr
graph export "${mainresultsdir}/appxfig7a.pdf", as(pdf) replace




capture drop udensity_prm
gen udensity_prm=upz_dist_prime
histogram udensity_prm

replace udensity_prm=. if udensity_prm<0.00003


#delimit ;
/*heatplot upz_dist z_level p_level,color(hcl, heat reverse) cuts(0.0002(0.0001)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) addplot(*/
heatplot udensity_prm z_level p_level,color(matplotlib hot, reverse) cuts(-0.0001(0.00005)0.006) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) 
xtitle("Aggegrate Productivity Level ({it:A})", size(vlarge)) ytitle(" {it:z}-Productivity Level", size(vlarge)) legend(off)
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==12, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(1) mlabgap(0)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) ylabel(0.4(0.2)1.0, labsize(large))
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 
ysize(3) xsize(4)

;
#delimit cr
graph export "${mainresultsdir}/appxfig7c.pdf", as(pdf) replace




//==============================================================================
// EMPLOYMENT
//==============================================================================


capture drop edensity
gen edensity=epz_dist 

replace edensity=. if edensity<0.0001

/*heatplot upz_dist z_level p_level,color(BuGn hcl, heat reverse) cuts(0.0002(0.0001)0.005) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) addplot(*/
/* colors: Blues */
#delimit ;
heatplot edensity z_level p_level,color(Blues) cuts(-0.00005(0.0004)0.022) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) 
xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("") legend(off) /*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(6) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==13, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==16, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==11, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) ylabel(0.4(0.2)1.0, labsize(large)) ysize(3) xsize(3.8)
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 

;
#delimit cr
graph export "${mainresultsdir}/fig7b.pdf", as(pdf) replace


capture drop edensityx
gen edensityx=epz_dist_x if x_index==1
replace edensityx=. if edensity<0.0001



capture drop edensity_yng
gen edensity_yng=epz_dist_young  
replace edensity_yng=. if edensity_yng<0.0001

capture drop edensity_prm
gen edensity_prm=epz_dist_prime  
replace edensity_prm=. if edensity_prm<0.0001

#delimit ;
heatplot edensity_yng z_level p_level,color(Greens) cuts(-0.00005(0.0002)0.03) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) 
xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("") legend(off) /*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(6) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==13, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==16, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==11, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) ylabel(0.4(0.2)1.0, labsize(large)) ysize(3) xsize(3.8)
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 

;
#delimit cr
graph export "${mainresultsdir}/appxfig7b.pdf", as(pdf) replace


#delimit ;
heatplot edensity_prm z_level p_level,color(Blues) cuts(-0.00005(0.0004)0.03) discrete(0.008) xdiscrete xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06) 
xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("") legend(off) /*legend(off)*/
/*ramp(right graphregion(color(white)) labels(none)) */
addplot(
scatter z_level p_level if z_r_binary==1 & x_index==1, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==1, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==18, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(6) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==1 & p_index==8, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(3)
||
scatter z_level p_level if z_r_binary==1 & x_index==2, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==2, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==13, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(2) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==2 & p_index==16, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(5) mlabgap(1)
||
scatter z_level p_level if z_r_binary==1 & x_index==3, connect(l) msymbol(i) lcolor(black) lwidth(medthick) 
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==11, msymbol(i ) mlab(zslabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(-1)
|| scatter z_level p_level if z_s_binary==1 & x_index==3 & p_index==18, msymbol(i ) mlab(zrlabel) mlabcolor(black) mlabsize(large) mlabposition(7) mlabgap(2)
|| scatter z_level p_level if z_s_binary==1 & x_index==3, connect(l l) msymbol(i i) lcolor(black) lwidth(medthick) lpattern(longdash)

xscale(r(0.94 1.06)) xlabel(0.94(0.02)1.06, labsize(large)) ylabel(0.4(0.2)1.0, labsize(large)) ysize(3) xsize(3.8)
graphregion(color(white))) /*name(cutoffs, replace) legend(off)*/ 

;
#delimit cr
graph export "${mainresultsdir}/appxfig7d.pdf", as(pdf) replace




//==============================================================================
// UNEMPLOYMENT DECOMPOSITION
//==============================================================================






// =====
** PROPORTIONS

capture drop up_search_prop
capture drop up_reall_prop
capture drop up_rest_prop

capture drop up_search_yng_prop
capture drop up_reall_yng_prop
capture drop up_rest_yng_prop

capture drop up_search_prm_prop
capture drop up_reall_prm_prop
capture drop up_rest_prm_prop

gen up_search_prop=up_search_dist/up_dist
gen up_reall_prop=up_reall_dist/up_dist
gen up_rest_prop=up_rest_dist/up_dist

gen up_search_yng_prop=up_search_dist_young/up_dist_young
gen up_reall_yng_prop=up_reall_dist_young/up_dist_young
gen up_rest_yng_prop=up_rest_dist_young/up_dist_young

gen up_search_prm_prop=up_search_dist_prime/up_dist_prime
gen up_reall_prm_prop=up_reall_dist_prime/up_dist_prime
gen up_rest_prm_prop=up_rest_dist_prime/up_dist_prime


sort p_level 
#delimit ;
scatter up_search_prop up_rest_prop up_reall_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/fig7c.pdf", as(pdf) replace



sort p_level 
#delimit ;
scatter up_search_prop up_rest_prop up_reall_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig6a.pdf", as(pdf) replace


sort p_level 
#delimit ;
scatter up_search_yng_prop up_rest_yng_prop up_reall_yng_prop  p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) cols(1) pos(3) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig6b.pdf", as(pdf) replace



sort p_level 
#delimit ;
scatter up_search_prm_prop up_rest_prm_prop up_reall_prm_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig6c.pdf", as(pdf) replace




********************************************************************************

*** FULL MODEL NET+EXCESS MOBILITY -- UNEMPLOYMENT DECOMPOSITION

********************************************************************************


import delimited "${fortrandir}/Gross and Net Mobility version/dist_bests10_000.csv", numericcols(66) clear 
global lversion netmob

capture drop up_search_prop
capture drop up_reall_prop
capture drop up_rest_prop

capture drop up_search_yng_prop
capture drop up_reall_yng_prop
capture drop up_rest_yng_prop

capture drop up_search_prm_prop
capture drop up_reall_prm_prop
capture drop up_rest_prm_prop

gen up_search_prop=up_search_dist/up_dist
gen up_reall_prop=up_reall_dist/up_dist
gen up_rest_prop=up_rest_dist/up_dist

gen up_search_yng_prop=up_search_dist_young/up_dist_young
gen up_reall_yng_prop=up_reall_dist_young/up_dist_young
gen up_rest_yng_prop=up_rest_dist_young/up_dist_young

gen up_search_prm_prop=up_search_dist_prime/up_dist_prime
gen up_reall_prm_prop=up_reall_dist_prime/up_dist_prime
gen up_rest_prm_prop=up_rest_dist_prime/up_dist_prime


sort p_level 
#delimit ;
scatter up_search_prm_prop up_rest_prm_prop up_reall_prm_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig4a.pdf", as(pdf) replace



sort p_level 
#delimit ;
scatter up_search_yng_prop up_rest_yng_prop up_reall_yng_prop  p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(3) cols(1) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig4b.pdf", as(pdf) replace



sort p_level 
#delimit ;
scatter up_search_prm_prop up_rest_prm_prop up_reall_prm_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 3 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig4c.pdf", as(pdf) replace



********************************************************************************

*** NO REALLOCATION -- MODEL I --  UNEMPLOYMENT DECOMPOSITION

********************************************************************************

import delimited "${fortrandir}/Robustness No Reallocation/No Reallocation Model I/dist_bestv16r1_34000.csv", numericcols(66) clear 
global lversion noreall1

capture drop up_search_prop
capture drop up_reall_prop
capture drop up_rest_prop

capture drop up_search_yng_prop
capture drop up_reall_yng_prop
capture drop up_rest_yng_prop

capture drop up_search_prm_prop
capture drop up_reall_prm_prop
capture drop up_rest_prm_prop

gen up_search_prop=up_search_dist/up_dist
gen up_reall_prop=up_reall_dist/up_dist
gen up_rest_prop=up_rest_dist/up_dist

gen up_search_yng_prop=up_search_dist_young/up_dist_young
gen up_reall_yng_prop=up_reall_dist_young/up_dist_young
gen up_rest_yng_prop=up_rest_dist_young/up_dist_young

gen up_search_prm_prop=up_search_dist_prime/up_dist_prime
gen up_reall_prm_prop=up_reall_dist_prime/up_dist_prime
gen up_rest_prm_prop=up_rest_dist_prime/up_dist_prime


sort p_level 
#delimit ;
scatter up_search_prop up_rest_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig9a.pdf", as(pdf) replace



********************************************************************************

*** NO REALLOCATION -- MODEL II --UNEMPLOYMENT DECOMPOSITION

********************************************************************************

import delimited "${fortrandir}/Robustness No Reallocation/No Reallocation Model II/dist_bestv16r1_34000.csv", numericcols(66) clear 
global lversion noreall2


// =====
** PROPORTIONS

capture drop up_search_prop
capture drop up_reall_prop
capture drop up_rest_prop

capture drop up_search_yng_prop
capture drop up_reall_yng_prop
capture drop up_rest_yng_prop

capture drop up_search_prm_prop
capture drop up_reall_prm_prop
capture drop up_rest_prm_prop

gen up_search_prop=up_search_dist/up_dist
gen up_reall_prop=up_reall_dist/up_dist
gen up_rest_prop=up_rest_dist/up_dist

gen up_search_yng_prop=up_search_dist_young/up_dist_young
gen up_reall_yng_prop=up_reall_dist_young/up_dist_young
gen up_rest_yng_prop=up_rest_dist_young/up_dist_young

gen up_search_prm_prop=up_search_dist_prime/up_dist_prime
gen up_reall_prm_prop=up_reall_dist_prime/up_dist_prime
gen up_rest_prm_prop=up_rest_dist_prime/up_dist_prime


sort p_level 
#delimit ;
scatter up_search_prop up_rest_prop p_level if p_level>0.95 & p_level<1.05, 
connect(l l l l) msymbol(Oh Sh Dh Th) msize(vlarge vlarge vlarge) mlwidth(thick thick thick) graphregion(color(white)) xtitle("Aggegrate Productivity Level ({it:A}) ", size(vlarge)) ytitle("Proportion of Unemp. Type", size(vlarge) ) legend(order(1 2 ) label(1 "Search") label(2 "Rest") label(3 "Reall.") ring(0) pos(12) size(*1.4)) xlabel(0.96(0.02)1.04, labsize(large)) ylabel(0(0.2)0.8, labsize(vlarge)) ysize(3) xsize(4) 
lcolor(sand*0.7 dkorange sienna*0.4) mcolor( sand dkorange sienna) lwidth(vthick vthick vthick thick) lpattern(shortdash solid longdash)
;
#delimit cr
graph export "${mainresultsdir}/appxfig9b.pdf", as(pdf) replace



global lendtime=c(current_time)
	global lenddate=c(current_date)
	display "ended at ${lendtime} on ${lenddate}"
	global lrunningtime=(clock("${lendtime}", "hms")-clock("${lstarttime}", "hms"))/1000 
	display "running time ${lrunningtime} seconds"