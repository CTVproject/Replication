# RESULTS DIRECTORY

Note: `${step2codedir}/`, `${cpsdir}`, etc refer to the paths set in `global_paths.do`. By default `${step2codedir}/` is the directory for the do-files that produce these results, so we suppress it at times for brevity. The results of the model, especially numerical information for tables, can be found in the main directory of each model version, typically at `${fortrandir}/[quantitative model version here]/`. However, do-file 
`${sec45codedir}compresults_5_furthertables.do` copies the information relevant for the paper to the results directory (`${mainresultsdir}`) and renames them so that it is clear for which part of the paper it is relevant. If a 'model' table is left without source, by default, this is how it came about. This results directory is not complete: not all results produced are included here, but hopefully this helps one on their way. 

## MAIN TEXT
  
### FIG 1

* `fig1a.pdf`, `fig1b.pdf` from `${step2codedir}/step2_1_durationprofiles.do`. 

### FIG 2

* `fig2.pdf` from `step2_2_netgrossmobility_occ_overall.do`
In the published version, we have added some labels to make the routine-based super-occupation categories visible in black and white. The figure is labeled `fig2_forgreyscale_paper.pdf'

### REPEAT MOBILITY STATS
produced in `step2_3_repeatmobility.do`

### TABLE 1

* `table1_regs_i_ii.txt`  from `step2_10_cycl_occupational_mobility.do`
* `table1_regs_iii_vi.txt` from `${cpsdir}/cps_regressions_maintext.do`
* `table1_regs_iv.txt` from `$step2_5_3_cycl_regressions_corrected_unfiltered.do` 
* `table1_regs_v_vii.txt` from `$step2_4_cycl_regressions_uncorrected_unfiltered.do`

### FIGURES 3

* `fig3a.pdf` from `step2_6_cyclical_durationprofile_shift.do`
* `fig3b.pdf` from `step2_7_1_netmobility.do`

### TABLE 2 (MOMENTS)
**EMPIRICAL DATA**

* `table2_appxtable4_ ts_outpw.xls`: properties of the aggregate production process
* `table2_mean_unemp.txt`: mean unemployment target (time series average unemployment stock of those who have earlier employment)
* `table2_panelA_rel_udur_moverstayer.xlsx`: durations of occ stayers vs movers
* `table2_appxtable4_separation_uconcentration.txt`: relative separation young/prime, relative separation recent hires/all employed, unemployment concentration (prop of now employed workers that will have some unemployment in next 3 years)
* `repeatmobility.xls`, measure *sas_prefmeasure_corr* is the repeat mobility: occ stay after stay measure 

The following moments are set based on the literature

* elasticity of matching function = 0.5 (consensus value)
* 5- and 10-year OLS returns to occupational tenure (Kambourov and Manovskii 2009)

The following files contain the information on the survival profiles and the occupational mobility/unemployment duration profiles

* `durationprofiles_mog.csv`: the mobility/duration profiles for all workers/young/prime-aged. From the young/prime-aged at month two, follows the calibration moment in panel A of table 2; the data part of figs 5a-b 
* `table2_occmob_rel_y_p_moment_model_data`: the additional occ mobility average young/prime relative occupational mobility profile (average of the profile of young/average of the profile of the prime-aged, weighing each months 1-12 equally. Both in model and data we use the measures that are not corrected for occupational miscoding, for the aggregate mobility profile, while the corrected ones for the young and prime-aged. 
* `cycldurationshift_mog.dta`: the mobility/duration profiles for times of high and low U; the data part of fig 5c
* `survivalprofiles_and_hazards.xls`: the survival profiles behind figures 5d-f

*SUPEROCC STATS* 

* `table2panelB_occ_change_data.txt`: end distribution, table 2, panel B
* `table2_panelB_transmat_netmob.xls`: transition matrix (table 2, panel B), net mobility over entire sample (table 2, Panel B), average gross mobility rate across the 4 task-based occupational categories (super-occupations; table 2 Panel A)  
* `table2_panelC_netflow_inflowdistr_cycleshift.xls`: netmob_bad, netmob_good, rec_exp_netflowshift are respectively the netflow rates for times of high u, netflow rates for times of low u, and the difference between the two. (Note, the sign on whether a positive net flow is a net inflows vs outflows, as the definition in the paper and in the calculation are flipped). inflowshift_exp_rec is the shift in the inflow distribution from expansion to recession. 
* `table2_table2appendix_superocc_udur_elasticity.txt` (renamed `table2_appxtable2_superocc_udur_elasticity.txt`in most recent version) contains the responsiveness of unemployment durations inside superoccs to aggregate unemployment. The numbers here are put into the computational model (and treated the same there as in the 'model data' calculation), in `mod_auxdata_moments.f90` (values .409815, .3836653, .2838073, .4183958). To see how the *normalized* empirical u. duration elasticities (with the aggregate unemployment rate), see below in the model data output, data column.  
    
**MODEL DATA**

* `table2_model` gives the model counterparts to the data moments above. The right-most column also reports the data counterpart, but these numbers are early input by hand, and do not change if the STATA output would be different. So, it is preferable to use the above STATA output for replication purposes.  

### TABLE 3 (PARAMETERS)

* `table3.txt`

### TABLE 4 (MAIN TIME SERIES PROPERTIES)

* `table4_appxtable3_data.txt` from `step2_11_5_cyclical_statistics_for_paper.do`
* `table4_excessmob_model.txt`
* `table4_grossnetmob_model.txt`

### TABLE 5 (DURATION DISTRIBUTION RESPONSES)

* `table5_panelB_udur_cycle_data.xls` produced in `step2_12_unemployment_durations.do`
* `table5A_appxtable7_durdistr_elas_data.txt` produced in `step2_12_unemployment_durations.do`
* `table5A_appxtable7_hpdurdistr_semielas_data.txt` produced in `step2_12_unemployment_durations.do`

* `table5_grossnetmob_model.txt`
* `table5_excessmob_model.txt`

### FIG 6

* `fig6.pdf` produced in `${sec45codedir}/compresults_1_durationprofiles.do`, using both empirical and model data.


### FIGURES 7

* `fig7a.pdf`, `fig7b.pdf` and `fig7c.pdf` produced in `${sec45codedir}compresults_3_distribution_figures_decomp.do`


### TABLE 6

* `table6.xlsx` (from compresults_2_appxfig3b_cycl_netflow_inflow_shifts.do`)
* `table6_explanation.txt`
* `table6_occ_change_data.txt` produced in `step2_7_2_occsize_change.do`


### FIGURES 8

* `fig8x.pdf` (with x=a,b,c,d,e,f) drawn in `${sec45codedir}compresults_4_distributionfigs_netmob.do`



## ONLINE APPENDIX 

### APPENDIX FIGURES 1, FIGURES 2

* `appxfig1a_all.pdf`,`appxfig1b_young.pdf`, `appxfig1c_prime.pdf`, `appxfig2a_all.pdf`, 
`appxfig2b_yng.pdf` and `appxfig2c_prime.pdf` show the job finding hazard rates in model and data, and are drawn in `${sec45codedir}/compresults_1_durationprofiles.do`

### APPENDIX TABLE 1
only the noncyclical parts of the data below; sources come back for appendix table 8 
*DATA*

* `appxtable1_appxtable7_incompl_durdistr_data.txt`

*MODEL*

* `appxtable1_excessmob_model.txt`
* `appxtable1_grossnetmob_model.txt`

### APPENDIX TABLE 2
 
 * *DATA* comes from `table2_table2appendix_superocc_udur_elasticity.txt`
 * *MODEL* comes from `table2_moments` for the normalized elasticities, but the raw elasticities are in `appxtable2_grossnetflow_stats.csv`, but only columns *udur_elas_raw_m* and *udur_elas_m* (other columns summarize other targeted moments of the full model)

### APPENDIX FIG 3

* `appxfig3a.pdf` drawn in `${sec45codedir}/compresults_1_durationprofiles.do`
* `appxfig3b.pdf` drawn in `${sec45codedir}/compresults_2_appxfig3b_cycl_netflow_inflow_shifts.do`

### APPENDIX TABLE 3

* `table4_appxtable3_data.txt` from `step2_11_5_cyclical_statistics_for_paper.do`
* `appxtable3_unsmoothed_data.txt`
* `appxtable3_grossnetmob_model.txt`
* `appxtable3_grossnetmob_model_unsmooth.txt`

### APPENDIX TABLE 4

* *MODEL* `appxtable4_model.txt`
* *DATA* see TABLE 2, added (in addition to visually in the pictures)
* `appxtable4_mobdurprofiles_xs_calibration.txt`
* `appxtable4_model.txt`
* `appxtable4_survprofile_xs_calibration.txt`

### APPENDIX FIG 4 (DECOMPOSITION UNEMPLOYMENT TYPES FULL MODEL)

* `appxfig4a.pdf`, `appxfig4b.pdf` and `appxfig4c.pdf` drawn in `${sec45codedir}compresults_3_distribution_figures_decomp.do`

### APPENDIX FIG 5 (MOBILITY/DURATION and SURVIVAL in the EXCESS MOBILITY MODEL)

*`appxfig5a.pdf`, `appxfig5b.pdf`, `appxfig5c.pdf`, `appxfig5d.pdf`, `appxfig5e.pdf` and `appxfig5f.pdf`, all drawn in `${sec45codedir}/compresults_1_durationprofiles.do`

### APPENDIX FIG 6 (DECOMPOSITION UNEMPLOYMENT TYPES EXCESS MODEL )

* `appxfig6a.pdf`, `appxfig6b.pdf` and `appxfig6c.pdf` drawn in `${sec45codedir}compresults_3_distribution_figures_decomp.do`

### APPENDIX FIG 7 (DECOMPOSITION UNEMPLOYMENT TYPES EXCESS MODEL )

* `appxfig7a.pdf`, `appxfig7b.pdf`, `appxfig7c.pdf` and `appxfig7d.pdf` produced in `${sec45codedir}compresults_3_distribution_figures_decomp.do`

### APPENDIX FIG 8 (MOBILITY DURATION PROFILES W/ and W/OUT SKILL DEP)

* `appxfig8a.pdf`
* `appxfig8b.pdf` both produced in  `${sec45codedir}/compresults_1_durationprofiles.do`

### APPENDIX TABLE 5


* `appxtable5_excessmob_model.txt`
* `appxtable5_xsmobnoskilldep_model.txt`


### APPENDIX TABLE 6  

* `appxtable6_excessmob_NUN_model.txt`
* `appxtable6_nun_data.txt` from `step2_11_5_cyclical_statistics_for_paper.do`



### APPENDIX TABLE 7 

* `appxtable7_noreall1_model.txt`
* `appxtable7_noreall2_model.txt`

### APPENDIX TABLE 8, UNEMPLOYMENT DURATION DISTRIBUTION STATS 

* `appxtable1_appxtable7_incompl_durdistr_data.txt`: *DATA*

*`appxtable8_grossnetmob_model.txt`
*`appxtable8_noreall1_model.txt`
*`appxtable8_noreall2_model.txt`


### APPENDIX FIGURES 9 

* `appxfig9a.pdf`
* `appxfig9b.pdf` both from `${sec45codedir}compresults_3_distribution_figures_decomp.do`


