# README MAIN Results directory

(CTV January 2023)

In this directory we list the main data moments/results of the paper, using the SIPP. Unless otherwise notes, each section number *x* corresponds to the do-file step2_*x*_[...] This is not a comprehensive overview of all files produced, but hopefully it gives a bit more of an idea how the replication package works. 

## 1. Duration profile (noncyclical). Fig 1a and 1b. Section 2.2

* FILES `durationprofiles_mog_nun_males.dta` and `durationprofiles_mog_nun_males.csv`
* FILE `durationprofiles_mog.dta` and `durationprofiles_mog.csv`
* FIGURES 1a, 1b

## 2. Excess and Net mobility, occ-spec mobility. Fig 2. Section 2.3

* FILE `net_gross_mobility_agg_and_occspec.xls`
    - ave_grossmob_mmo: the average propensity of an unemployed to change (2000 SOC MOG) occupation at the end of their unemployment spell
    - netreallrate_mmo, per occ: ((# of u spells starting in occupation o, ending in another occupation)-(# of u spells ending in occupation o, starting elsewhere))/((# of u spells starting in occupation o, ending in another occupation)+(# of u spells ending in occupation o, starting elsewhere)) 
    - netflow_all_u_mmo: (sum (over all occupations) of (inflows from elsewhere into an occupation + outflow to elsewhere from an occupation))/(2 * sum  unemployment spells)  	
    - netflows_mmo, per occ: (# of u spells starting in occupation o, ending in another occupation)-(# of u spells ending in occupation o, starting elsewhere)/(sum all u spells)
    - scaled_netflow_mmo, per occ:  ((# of u spells ending in occupation o, starting elsewhere)-(# of u spells starting in occupation o, ending in another occupation))/(# of u spells starting in occupation 0, including occupational stays)
    - ave_netflowprop_mmo: netflow_all_u_mmo/(ave_agrossmob_mmo)

* 4 measures:
    - with and without spells that involve management as a source/destination occupation
    - '_mw', instead of only considering spells of workers who are at least 14 months in sample upon completion, we consider spells of workers who are at least 4 months in sample. This increases the set of eligible spells substantially, but includes above-proportionally (relative to the cross section) short spells. A large set of spells may be desirable for relatively accurate measurement of (the small) net mobility rates. We therefore opt for the '_mw' measure as the default one (and shown in fig 2). For robustness we include a version of fig 2 with the more stringent censoring restriction, which does not change the picture in terms of overall mobility rates, the importance of excess mobility, the net mobility patterns across the 4 (routine-based) super-occupations. 

* FIGURE 2

## 3. Repeat Mobility. section 2.4

* FILE `repeatmobility.dta` and FILE `repeatmobility.xls`
    -  sas_prefmeasure_corr	gives the probability of an occupational stay in the subsequent spell following an earlier occupational stay (after applying the $\Gamma$-correction)
    - mam_prefmeasure_corr gives gives the probability of an occupational move in the subsequent spell following an earlier occupational move (again, after applying the $\Gamma$-correction)


## 4. Uncorrected Time Series Regressions, section 2.5, part 1

* FILE `table1_regs_v_vii.txt` 

    Reports the regressions behind columns (v) and (vii) of Table 1 in the main paper. Columns (i), (ii) and (iv) are produced after the quarterly time series have been calculated, with (i) and (ii) further after HP filtering these series. 

## 5. Corrected Timeseries for Occupational Mobility and Occupational Mobility 

### 5.1 Occupational Mobility Timeseries Creation

* `${outputdata}/timeseriesdur_mm.xls`: Quarterly series of occupational mobility per unemployment duration
* `${outputdata}/timeseries_mm.xls`: Quarterly series of occupational mobility

### 5.2 Occupational Mobility Timeseries ready for TRAMO/SEATS
This do-file (following the results of step2_5_1) produces the occupational mobility series that can be entered into TSW+ to apply TRAMO/SEATS. The list of 
* `${outputdata}/occmob_ts_for_tramo.xlsx`
* `${outputdata}/occmob_ts_for_tramo.dta`

Only the series we need for the paper, in
* `${outputdata}/ts_occmob_paper_for_tramo.xlsx`
    - qn1_mob_12_mm:  one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
    - qn1u_mob_12_mm:  one quarter, unsmoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure unemployment spells only
    - qn3_mob_12_mm: 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, GAMMA CORRECTED, pure unemployment spells only
    - qn3u_mob_12_mm 5 quarter rolling window smoothed, occ mobility all workers, 2000 MOG, durations <=12m, NOT CORRECTED FOR OCC MISCODING, pure 
    - lunrate_bls: for a robustness exercise in step2_10 (TRAMO/SEATS trendcycle component unemployment rate ) 

### 5.3 corrected time series regression, section 2.5, part 2

* FILE `table1_regs_iv.txt`

## 6. Cyclical Duration Shift, section 2.5 and figure 6 (data part)

The occupational mobility / unemployment duration profile shifts across the cycle. This is captured in the following files 

* FILE `cycldurationshift_mog.dta`
* FILE `cycldurationshift_mog_nun_males.dta`
(the latter is produced in **step2_1**!)
* FILE `fig6data_cycldurshift_ptiles.dta`, contains the data part of figure 6

* FIGURE 3a
* an alternative, data-only version of FIGURE 6 





## 7. Net Mobility, and the change in occupation sizes


### 7.1 net mobility

for section 2.5, and in calibration section 4 (table 2)

* FIGURE 3b
* FILE `table2_panelB_transmat_netmob.dta` and `table2_panelB_transmat_netmob.xls` contain the transition matrix across the four routine occupatons, and the overall net mobility per occupation, and overall gross mobility.
* FILE `table2_panelC_netflow_inflowdistr_cycleshift.dta` and `table2_panelC_netflow_inflowdistr_cycleshift.xls` contain the cyclical net mobility (also used in figure 3b) and the inflow distribution shift


### 7.2 evolution of occupation sizes

for calibration (section 4, table 2) and table 6 
* `table2panelB_occ_change_data.txt` and `table6_occ_change_data.txt` both contain the start (1984) and ending (2012) distribution  

* FILE `occdistr_1984_2013_corr.xls` in the outputdata directory contains the complete evolution of the occupational  distributions


## 8. Survival Profile and Other Remaining Pooled SIPP Calibration Inputs/Targets 

### 8.1 Survival Profiles 
This do-file produces the survival profiles with unemployment duration, job finding hazard p. unemployment duration, also split out by (ex-post) movers and stayers. It outputs:

* `survivalprofiles_and_hazards.dta` and `survivalprofiles_and_hazards.xls`, which are later used in the production of Figure 5 and Appendix Figure 5. 

### 8.2 Other calib targets calculated over the pooled sample.
This covers the various separation targets, and the 'unemployment concentration target'. It also calculates the miscoding propensities at the Task-Based Occupational Category (i.e. super-occupation) level that are used directly in our quantitative model.  

* `table2_appxtable4_separation_uconcentration.txt`

* `calib_miscodingstats.txt`

## 9. Other Time Series creation

### 9.1 Unemployment and 'NUN'-employment 



* `table2_mean_unemp.txt`: mean unemp level calibration 
* `calib_mean_nunemp_xsmob_nun.txt`: mean NUN/(NUN+E) rate, NUN-calibration
* `timeseries_u.dta`
* `u_ts_for_tramo.xlsx`, created in the output directory; optional *not part of main run*, instead of TRAMO, we use the bls unemployment rate to fill up the gaps of the unemployment series of focus (the stock of those unemployment who have been employed before in sample)
																		*/
		
        

			
### 9.2 Job Finding

* `${outputdata}/ts_jf_paper_before_tramo.xlsx`, sheet(`jf`)
* `${outputdata}/ts_jf_paper_before_tramo.dta` only those series used in the paper, which are
    - *ts_ljf_q*: quarterly timeseries of default job finding measure for the unemployed
	- *q3_ljf_q*: smoothed quarterly timeseries of default job finding measure for the unemployed
	- *q3_ljfn_cnune_q*:  job finding rate from NUN spells, 5q-smoothed
	- *q3_ljfn_cnune_instr_q*: because censoring matters a great deal with the NUN job finding rate, we have large gaps (and noise), we instrument this series by the BLS job finding rate (see `${aggdatadir}/README.txt` for info) 
	
	
### 9.3 Separations
The main time series is taken to TRAMO/SEATS to deal with the timeseries gaps.  

* `${outputdata}/ts_sep_paper_for_tramo.xlsx`:  the series ts_lsep_q and q3_lsep_q that, after tramo, are used in step2_11
* `${outputdata}/ts_sep_for_tramo.xlsx` and `ts_sep_for_tramo.dta` contain a larger set of separation time series, including across ages, that can also be Tramo-ed, if interested

								
## 10. (Filtered) Occupational Mobility

 
* TABLE 1 columns (i)-(ii) `table1_regs_i_ii.txt`
* `${outputdata}/occmob_ts_after_tramo.dta` used in step2_11_5	
																				*/
		
				
## 11. (Filtered) Time Series Stats for paper

* Step2_11_1 PRODUCES `u_hp_ts2020.dta` in outputdir, used in step2_11_5 and `timeseries_u_durwvcearly.dta` in outputdir. The latter is used also in step2_12. Uses the BLS unemployment series to fill in the gaps of the SIPP unemployment series. (Alternative, which is commented out: use TRAMO/SEATS to fill in the gaps)

* Step2_11_2 PRODUCES `jf_hp_ts2020.dta` in outputdir, used in step2_11_5. READS in the series that were TRAMO-ed before. 

* Step2_11_3 PRODUCES `sep_hp_ts2020.dta` in outputdir, used in step2_11_5. READS in the series that were TRAMO-ed before. 
						
* Step2_11_4 PRODUCES `nun_ts2020.dta` in outputdir, used in step2_11_5. Like step2_11_1, we use the BLS unemployment rate here to help us fill in the gaps. 

* step2_11_5 uses the aforementioned dtas to produce the time series cross-correlations and other properties, for TABLE 4, APPENDIX TABLES 3 and 5

* `appxtable3_unsmoothed_data.txt`
* `appxtable6_nun_data.txt`
* `table4_appxtable3_data.txt`

## 12 Unemployment Durations

* `${mainresultsdir}/text_u_durations_comparison.txt` comparisons made in the first two sections of the main text 
* `table2_panelA_rel_udur_moverstayer.xlsx`
* `table5_panelB_udur_cycle_data.xls`
* `table2_appxtable2_superocc_udur_elasticity_data.txt`
* `appxtable1_appxtable8_incompl_durdistr_data.txt`
* `table5A_appxtable8_durdistr_elas_data.txt`
* `table5A_appxtable8_hpdurdistr_semielas_data.txt`

## Computational Results Processing 1.
From file `${sec45codedir}/compresults_1_durationprofiles.do`

* FIGS 5a-f
* FIGURE 6
* APPENDIX FIGS 1a-c, 2a-c, 3a, 5a-f, 8a-b

* `appxtable4_mobdurprofiles_xs_calibration.txt`
* `appxtable4_survprofile_xs_calibration.txt`
* `appxtable4_occmob_rel_y_p_moment_model_data.log`
			
* `xtra_mobdurprofile_grossnet_calibration.txt`
* `xtra_survprofile_grossnet_calibration.txt`
* `xtra_survprofile_xsnoskilldep_calibration.txt`
* `xtra_mobdurprofiles_xsnoskilldep_calibration.txt` 
* `xtra_mobdurprofiles_xsnun_calibration.txt`
* `xtra_survprofile_xsnun_calibration.txt`
* `xtra_survprofile_noreall1_calibration.txt`
* `xtra_survprofile_noreall2_calibration.txt`

## Computational Results Processing 2: Shifts over the Cycle, and over the course of the entire time window

Done in FILE `${sec45codedir}/compresults_2_appxfig3b_cycl_netflow_inflow_shifts.do`

* appxtable2_model.txt
* APPENDIX FIGURE 3B
* `table6.xlsx`
* `table6_explanation.txt`

## Computational Results Processing 3: Type of unemployment, decomposition
`${sec45codedir}/compresults_3_distribution_figures_decomp.do`

Heatmaps:

* `fig7a.pdf`
* `fig7b.pdf`

* `appxfig7a.pdf`
* `appxfig7b.pdf`

* `appxfig7d.pdf`
* `appxfig7e.pdf`


Decomposition:


* `fig7c.pdf`
* `appxfig4a.pdf`
* `appxfig4b.pdf`
* `appxfig4c.pdf`
* `appxfig9a.pdf`
* `appxfig9b.pdf`
* `appxfig7c.pdf`
* `appxfig6a.pdf`
* `appxfig6b.pdf`
* `appxfig6c.pdf`

## Computational Results Processing 4: Heterogeneity across Super-Occs

* FIGURES 8a-f in the main text


## Computational Results Processing 5: written output
Copies the relevant written output files of the different calibrations to the results section (with hopefully relevant names)

* `appxtable5_xsmobnoskilldep_model.txt`
* `appxtable6_excessmob_NUN_model.txt`
* `appxtable7_noreall1_model.txt`
* `appxtable7_noreall2_model.txt`
* `appxtable8_excessmob_model.txt`
* `appxtable8_grossnetmob_model.txt`
* `appxtable8_noreall1_model.txt`
* `appxtable8_noreall2_model.txt`
* `appxtext_noreall1_parameter_model.txt`
* `appxtext_noreall2_parameter_model.txt`
* `appxtext_parameters_excessmob_model.txt`
* `table2_model.txt`
* `table3.txt`
* `table4_excessmob_model.txt`
* `table4_grossnetmob_model.txt`
* `table5_excessmob_model.txt`
* `table5_grossnetmob_model.txt`
* `text_excessmob_calibmodelstatements.txt`
* `text_grossnetmob_calibmodelstatements.txt`
* `xtra_excessmob_NUN_model_parameters.txt`
* `xtra_xsmobnoskilldep_model_parameters.txt`

* 
* `appxtable1_excessmob_model.txt`
* `appxtable1_grossnetmob_model.txt`
* `appxtable3_grossnetmob_model.txt`
* `appxtable3_grossnetmob_model_unsmooth.txt`
* `appxtable4_model.txt`
* `appxtable5_excessmob_model.txt`

