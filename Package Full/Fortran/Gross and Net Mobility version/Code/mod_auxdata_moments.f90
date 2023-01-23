!----------------------------------------------------------------
! This module contains data moments not used in the estimation, LV 2018
!   but rather to evaluate the performance of the model
!
!  LV 4/2018
!---------------------------------------------------------------


MODULE mod_auxdata_moments

    !! MAKE SURE OCCPTS EQUALS 4
    !INTEGER, PARAMETER :: 4=4

    !! EXTERNALLY SET MOMENTS
    REAL(8), DIMENSION(4)   :: init_occdistr
    REAL(8), DIMENSION(4,4) ::  gamma_miscode, invgamma_miscode
    REAL(8) ::gamma_misc_within_superocc_data
              

    !! MODEL INTERNAL (INDIRECT) TARGETS AND OUTCOMES

   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_data, tot_durationmatrix_cocc_data , tot_durationmatrix_nocc_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_young_data, tot_durationmatrix_cocc_young_data , tot_durationmatrix_nocc_young_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_prime_data, tot_durationmatrix_cocc_prime_data , tot_durationmatrix_nocc_prime_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_data, tot_durationmatrix_cr_cocc_data , tot_durationmatrix_cr_nocc_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_young_data, tot_durationmatrix_cr_cocc_young_data , tot_durationmatrix_cr_nocc_young_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_prime_data, tot_durationmatrix_cr_cocc_prime_data , tot_durationmatrix_cr_nocc_prime_data
   REAL(8), DIMENSION(0:24) :: tot_udurationmatrix_m_young_data, tot_udurationmatrix_s_young_data, tot_udurationmatrix_m_data
   REAL(8), DIMENSION(0:24) :: tot_udurationmatrix_m_prime_data, tot_udurationmatrix_s_prime_data, tot_udurationmatrix_s_data

   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix_data, tot_haz_durationmatrix_cocc_data  , tot_haz_durationmatrix_nocc_data
   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix_young_data, tot_haz_durationmatrix_cocc_young_data  , tot_haz_durationmatrix_nocc_young_data
   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix_prime_data, tot_haz_durationmatrix_cocc_prime_data  , tot_haz_durationmatrix_nocc_prime_data
   REAL(8), DIMENSION(0:24) :: tot_haz_udurationmatrix_m_young_data, tot_haz_udurationmatrix_s_young_data , tot_haz_udurationmatrix_m_data
   REAL(8), DIMENSION(0:24) :: tot_haz_udurationmatrix_m_prime_data, tot_haz_udurationmatrix_s_prime_data , tot_haz_udurationmatrix_s_data

   REAL(8) :: reemployhaz_sam_data, reemployhaz_mas_data, reemployhaz_mam_data
   REAL(8) :: reemployhaz_sas_data
   REAL(8) :: reemployhaz_sas_y_data, reemployhaz_sam_y_data, reemployhaz_mas_y_data, reemployhaz_mam_y_data
   REAL(8) :: reemployhaz_sas_p_data, reemployhaz_sam_p_data, reemployhaz_mas_p_data, reemployhaz_mam_p_data

   REAL(8), DIMENSION(0:7) :: duration2_data
   REAL(8), DIMENSION(0:7) :: duration2_young_data, duration2_prime_data
   REAL(8), DIMENSION(0:7) :: duration2_move_data, duration2_stay_data, duration2_move_young_data, duration2_stay_young_data, duration2_move_prime_data, duration2_stay_prime_data
   
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix2_cocc_data,  tot_durationmatrix2_cocc_young_data,  tot_durationmatrix2_cocc_prime_data
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix2_nocc_data,  tot_durationmatrix2_nocc_young_data,  tot_durationmatrix2_nocc_prime_data

   REAL(8) :: uproportion_window_data, uproportion_window_y_data, uproportion_window_p_data, uoccproportion_window_data, unoccproportion_window_data

   REAL(8) :: cm_data, lcmy_lcmp_data, relmob_data, cmy_cmp_data, cmy_cmp_corr_data, cmy_cmp_data2m, cmy_cmp_corr_data2m, cmy_cmp_data3m, cmy_cmp_corr_data3m, cmy_cmp_data4m, cmy_cmp_corr_data4m, cmy_cmp_data_ave, cmy_cmp_corr_data_ave
   REAL(8) :: cmy_cmp_mom, cmy_cmp_corr_mom, cmy_cmp_mom2m, cmy_cmp_corr_mom2m, cmy_cmp_mom3m, cmy_cmp_corr_mom3m, cmy_cmp_mom4m, cmy_cmp_corr_mom4m, cmy_cmp_mom_ave, cmy_cmp_corr_mom_ave
   REAL(8) :: ac_p_data, sd_p_data, elmatch_data, u_data, cmaftercm_cmaftercs_data, u_y_data, u_p_data, surv4m_data, surv8m_data
    REAL(8) :: surv12m_data, surv16m_data, rtnoccten5yr_data, rtnoccten10yr_data
    REAL(8) :: elmatch_lindt_ave_data, el_u_with_v_lindt_data, el_u_with_p_lindt_data, el_u_with_v_data, el_u_with_p_data

    ! and from the data
    REAL(8) ::repeat_udur_corr_data, repeat_udur_el_data, repeat_udur_corr_m_data, repeat_udur_el_m_data, repeat_udur_corr_s_data, repeat_udur_el_s_data
    REAL(8) :: repeat_udur_corr_sam_data, repeat_udur_el_sam_data, repeat_udur_corr_sas_data, repeat_udur_el_sas_data
    REAL(8) :: repeat_udur_corr_mam_data, repeat_udur_el_mam_data, repeat_udur_corr_mas_data, repeat_udur_el_mas_data
    REAL(8) ::repeat_udur_corr_y_data, repeat_udur_el_y_data, repeat_udur_corr_m_y_data, repeat_udur_el_m_y_data, repeat_udur_corr_s_y_data, repeat_udur_el_s_y_data
    REAL(8) :: repeat_udur_corr_sam_y_data, repeat_udur_el_sam_y_data, repeat_udur_corr_sas_y_data, repeat_udur_el_sas_y_data
    REAL(8) :: repeat_udur_corr_mam_y_data, repeat_udur_el_mam_y_data, repeat_udur_corr_mas_y_data, repeat_udur_el_mas_y_data
    REAL(8) ::repeat_udur_corr_p_data, repeat_udur_el_p_data, repeat_udur_corr_m_p_data, repeat_udur_el_m_p_data, repeat_udur_corr_s_p_data, repeat_udur_el_s_p_data
    REAL(8) :: repeat_udur_corr_sam_p_data, repeat_udur_el_sam_p_data, repeat_udur_corr_sas_p_data, repeat_udur_el_sas_p_data
    REAL(8) :: repeat_udur_corr_mam_p_data, repeat_udur_el_mam_p_data, repeat_udur_corr_mas_p_data, repeat_udur_el_mas_p_data

    REAL(8)  :: noccafterocc_stu_data, noccafternocc_stu_data, noccafterocc_ltu_data, noccafternocc_ltu_data
    REAL(8), DIMENSION(0:7) :: empduration_data, empduration_y_data, empduration_p_data
    REAL(8), DIMENSION(0:7) :: empduration_shuspell_data, empduration_luspell_data
    REAL(8), DIMENSION(0:7) :: empduration_occ_data, empduration_nocc_data, empduration_occ_y_data, empduration_occ_p_data, empduration_nocc_y_data, empduration_nocc_p_data
    REAL(8), DIMENSION(0:7) :: empduration_occ_shuspell_data, empduration_nocc_shuspell_data, empduration_occ_luspell_data, empduration_nocc_luspell_data

    REAL(8), DIMENSION(4) :: tot_noccafterocc_vector_data, tot_noccafternocc_vector_data
    REAL(8), DIMENSION(4) :: tot_noccafterocc_vector_young_data, tot_noccafternocc_vector_young_data
    REAL(8), DIMENSION(4) :: tot_noccafterocc_vector_prime_data, tot_noccafternocc_vector_prime_data


   REAL(8) :: poccafterocc_data, poccafternocc_data,  poccafterocc_y_data, poccafternocc_y_data, poccafterocc_p_data, poccafternocc_p_data, pnoccafterocc_data
   REAL(8) :: pnoccafternocc_data, pnoccafterocc_y_data, pnoccafternocc_y_data, pnoccafterocc_p_data, pnoccafternocc_p_data

   REAL(8), DIMENSION(2)  :: poccafterocc_uspell_data, poccafternocc_uspell_data, poccafterocc_y_uspell_data, poccafternocc_y_uspell_data, poccafterocc_p_uspell_data, poccafternocc_p_uspell_data
   REAL(8), DIMENSION(2)  :: pnoccafterocc_uspell_data, pnoccafternocc_uspell_data, pnoccafterocc_y_uspell_data, pnoccafternocc_y_uspell_data, pnoccafterocc_p_uspell_data, pnoccafternocc_p_uspell_data


   REAL(8) :: jf_ave_data, jf_ave2_data, jfo_ave_data,  sep_ave2_data, v_ave_data, &
                    v_ave2_data, theta_ave_data, theta_ave2_data, z_ave_data, reall_ave_conditional_data, u_ave_data
   REAL(8), DIMENSION(4) :: unemp_supocc_ave_data

  REAL(8) :: noccafternocc_ave_data, noccafterocc_ave_data, noccafternocc_y_ave_data, noccafterocc_y_ave_data, noccafternocc_p_ave_data, noccafterocc_p_ave_data
  REAL(8) :: noccafterall_ave_data, noccafterall_y_ave_data, noccafterall_p_ave_data
  REAL(8) :: tot_corr_noccafternocc_data, tot_corr_noccafterocc_data, tot_corr_noccafternocc_y_data, tot_corr_noccafterocc_y_data, tot_corr_noccafternocc_p_data, tot_corr_noccafterocc_p_data
  REAL(8) :: tot_corr_noccafterall_data, tot_corr_noccafterall_y_data, tot_corr_noccafterall_p_data
  
  REAL(8) :: jf_young_ave_data, jf_prime_ave_data      !jfo_ave_data already defined!!!
  REAL(8) :: jfocc_ave_data, jfnocc_ave_data, jfocc_y_ave_data, jfnocc_y_ave_data, jfocc_p_ave_data, jfnocc_p_ave_data

  ! SEPARATION (OVERALL)
   REAL(8) :: sep_ave_1m_data, sep_ave_2m_data, sep_ave_3m_data,sep_ave_4m_data, sep_ave_5m_data, sep_ave_6m_data, sep_ave_7m_data, sep_ave_8m_data, sep_ave_9m_data, sep_ave_10m_data, sep_ave_11m_data, sep_ave_12m_data
   REAL(8) :: sepyoung_ave_data, sepprime_ave_data, sep_ave_data
   REAL(8) :: sepocc_afternocc_2_5yr_data, sepnocc_afternocc_2_5yr_data, sepocc_afternocc_3yr_data, sep_afternocc_2_5yr_data
   REAL(8) :: sepocc_ave_data, sepnocc_ave_data, sepocc_y_ave_data, sepnocc_y_ave_data, sepocc_p_ave_data, sepnocc_p_ave_data
   REAL(8) :: sep_ave_1m_posthire_data, sep_ave_2m_posthire_data, sep_ave_3m_posthire_data,sep_ave_4m_posthire_data, sep_ave_5m_posthire_data, sep_ave_6m_posthire_data, sep_ave_7m_posthire_data, sep_ave_8m_posthire_data, sep_ave_9m_posthire_data, sep_ave_10m_posthire_data, sep_ave_11m_posthire_data, sep_ave_12m_posthire_data
   REAL(8) :: sep_12m_ave_data, sepyoung_12m_ave_data, sepprime_12m_ave_data



     ! BUSINESS CYCLE ELASTICITIES -- DATA
   REAL(8) :: unemp_prod_elasticity_data, u_young_prod_elasticity_data, u_prime_prod_elasticity_data, sep_young_prod_elasticity_data, sep_prime_prod_elasticity_data
   REAL(8) :: cocc_inflow_prod_elasticity_data, cocc_inflow_unemp_elasticity_data, jf_young_prod_elasticity_data, jf_prime_prod_elasticity_data, jf_prod_elasticity_data, sep_prod_elasticity_data
   REAL(8) :: cocc_young_inflow_prod_elasticity_data, cocc_prime_inflow_prod_elasticity_data, cocc_young_inflow_unemp_elasticity_data, cocc_prime_inflow_unemp_elasticity_data
   REAL(8) :: sep_young_unemp_elasticity_data, sep_prime_unemp_elasticity_data, wage_prod_elasticity_data, jfocc_prod_elasticity_data, jfnocc_prod_elasticity_data
   REAL(8) :: jfocc_unemp_elasticity_data, jfnocc_unemp_elasticity_data
   REAL(8) :: jf_young_unemp_elasticity_data, jf_prime_unemp_elasticity_data, jf_unemp_elasticity_data, sep_unemp_elasticity_data
   REAL(8) :: sep_young_prod_semielasticity_data, sep_prod_semielasticity_data, sep_prime_prod_semielasticity_data, jfocc_prod_semielasticity_data, jfnocc_prod_semielasticity_data
   REAL(8) :: jf_prod_semielasticity_data, jf_young_prod_semielasticity_data, jf_prime_prod_semielasticity_data
   REAL(8) :: sep_young_unemp_semielasticity_data, sep_unemp_semielasticity_data, sep_prime_unemp_semielasticity_data, jfocc_unemp_semielasticity_data, jfnocc_unemp_semielasticity_data
   REAL(8) :: jf_unemp_semielasticity_data, jf_young_unemp_semielasticity_data, jf_prime_unemp_semielasticity_data
   !REAL(8) :: uprop5_prod_elasticity_data, uprop15_prod_elasticity_data, uprop27_prod_elasticity_data, upropgt27_prod_elasticity_data
   !REAL(8) :: uprop5_prod_semielasticity_data, uprop15_prod_semielasticity_data, uprop27_prod_semielasticity_data, upropgt27_prod_semielasticity_data
   !REAL(8) :: uprop5_unemp_elasticity_data, uprop15_unemp_elasticity_data, uprop27_unemp_elasticity_data, upropgt27_unemp_elasticity_data
   !REAL(8) :: uprop5_unemp_semielasticity_data, uprop15_unemp_semielasticity_data, uprop27_unemp_semielasticity_data, upropgt27_unemp_semielasticity_data
   REAL(8) :: jfocc5_prod_elasticity_data, jfnocc5_prod_elasticity_data, jf5_young_prod_elasticity_data, jf5_prime_prod_elasticity_data, jf5_prod_elasticity_data
   REAL(8) :: jfocc5_prod_semielasticity_data, jfnocc5_prod_semielasticity_data, jf5_young_prod_semielasticity_data, jf5_prime_prod_semielasticity_data, jf5_prod_semielasticity_data
   REAL(8) :: jfocc5_unemp_semielasticity_data, jfnocc5_unemp_semielasticity_data, jf5_young_unemp_semielasticity_data, jf5_prime_unemp_semielasticity_data, jf5_unemp_semielasticity_data
   REAL(8) :: jfocc5_unemp_elasticity_data, jfnocc5_unemp_elasticity_data, jf5_young_unemp_elasticity_data, jf5_prime_unemp_elasticity_data, jf5_unemp_elasticity_data
   !REAL(8) :: jf_prod_semielasticity_data, jfocc_prod_semielasticity_data, jfnocc_prod_semielasticity_data, jf_unemp_semielasticity_data, jfocc_unemp_semielasticity_data, jfnocc_unemp_semielasticity_data
   REAL(8) :: var_u_young_data, var_uprime_data, varjf_data, var_jfocc_data, var_jfnocc_data, var_jfyoung_data, var_jfprime_data, var_sepyoung_data, var_lsepyoung_data, var_lsepprime_data
   REAL(8) :: var_cm_data, var_sepprime_data, var_cmyoung_data, var_cmprime_data
   REAL(8) :: cocc_outflow_prod_elasticity_data, cocc_young_outflow_prod_elasticity_data, cocc_prime_outflow_prod_elasticity_data
   REAL(8) :: cocc_outflow_unemp_elasticity_data, cocc_young_outflow_unemp_elasticity_data, cocc_prime_outflow_unemp_elasticity_data
   REAL(8) :: cocc_outflow_prod_semielasticity_data, cocc_young_outflow_prod_semielasticity_data, cocc_prime_outflow_prod_semielasticity_data
   REAL(8) :: cocc_outflow_unemp_semielasticity_data, cocc_young_outflow_unemp_semielasticity_data, cocc_prime_outflow_unemp_semielasticity_data


    ! u-durations (in wks)
   REAL(8) :: u5w_prod_elasticity_data, u15w_prod_elasticity_data, u27w_prod_elasticity_data, ugt27w_prod_elasticity_data
   REAL(8) :: u5w_prod_semielasticity_data, u15w_prod_semielasticity_data, u27w_prod_semielasticity_data, ugt27w_prod_semielasticity_data
   REAL(8) :: u5w_unemp_elasticity_data, u15w_unemp_elasticity_data, u27w_unemp_elasticity_data, ugt27w_unemp_elasticity_data
   REAL(8) :: u5w_unemp_semielasticity_data, u15w_unemp_semielasticity_data, u27w_unemp_semielasticity_data, ugt27w_unemp_semielasticity_data
   REAL(8) :: uprop5w_prod_elasticity_data, uprop15w_prod_elasticity_data, uprop27w_prod_elasticity_data, upropgt27w_prod_elasticity_data
   REAL(8) :: uprop5w_prod_semielasticity_data, uprop15w_prod_semielasticity_data, uprop27w_prod_semielasticity_data, upropgt27w_prod_semielasticity_data
   REAL(8) :: uprop5w_unemp_elasticity_data, uprop15w_unemp_elasticity_data, uprop27w_unemp_elasticity_data, upropgt27w_unemp_elasticity_data
   REAL(8) :: uprop5w_unemp_semielasticity_data, uprop15w_unemp_semielasticity_data, uprop27w_unemp_semielasticity_data, upropgt27w_unemp_semielasticity_data
   REAL(8) :: luprop5w_prod_elasticity_data, luprop15w_prod_elasticity_data, luprop27w_prod_elasticity_data, lupropgt27w_prod_elasticity_data
   REAL(8) :: luprop5w_prod_semielasticity_data, luprop15w_prod_semielasticity_data, luprop27w_prod_semielasticity_data, lupropgt27w_prod_semielasticity_data
   REAL(8) :: luprop5w_unemp_elasticity_data, luprop15w_unemp_elasticity_data, luprop27w_unemp_elasticity_data, lupropgt27w_unemp_elasticity_data
   REAL(8) :: luprop5w_unemp_semielasticity_data, luprop15w_unemp_semielasticity_data, luprop27w_unemp_semielasticity_data, lupropgt27w_unemp_semielasticity_data
    ! u-durations (in wks)
   REAL(8) :: u3m_prod_elasticity_data, u5m_prod_elasticity_data, u9m_prod_elasticity_data, u13m_prod_elasticity_data, ug13m_prod_elasticity_data
   REAL(8) :: u3m_prod_semielasticity_data, u5m_prod_semielasticity_data, u9m_prod_semielasticity_data, u13m_prod_semielasticity_data, ug13m_prod_semielasticity_data
   REAL(8) :: u3m_unemp_elasticity_data, u5m_unemp_elasticity_data, u9m_unemp_elasticity_data, u13m_unemp_elasticity_data, ug13m_unemp_elasticity_data
   REAL(8) :: u3m_unemp_semielasticity_data, u5m_unemp_semielasticity_data, u9m_unemp_semielasticity_data, u13m_unemp_semielasticity_data, ug13m_unemp_semielasticity_data
   REAL(8) :: uprop3m_prod_elasticity_data, uprop5m_prod_elasticity_data, uprop9m_prod_elasticity_data, uprop13m_prod_elasticity_data, upropg13m_prod_elasticity_data
   REAL(8) :: uprop3m_prod_semielasticity_data, uprop5m_prod_semielasticity_data, uprop9m_prod_semielasticity_data, uprop13m_prod_semielasticity_data, upropg13m_prod_semielasticity_data
   REAL(8) :: uprop3m_unemp_elasticity_data, uprop5m_unemp_elasticity_data, uprop9m_unemp_elasticity_data, uprop13m_unemp_elasticity_data, upropg13m_unemp_elasticity_data
   REAL(8) :: uprop3m_unemp_semielasticity_data, uprop5m_unemp_semielasticity_data, uprop9m_unemp_semielasticity_data, uprop13m_unemp_semielasticity_data, upropg13m_unemp_semielasticity_data
   REAL(8) :: luprop3m_prod_elasticity_data, luprop5m_prod_elasticity_data, luprop9m_prod_elasticity_data, luprop13m_prod_elasticity_data, lupropg13m_prod_elasticity_data
   REAL(8) :: luprop3m_prod_semielasticity_data, luprop5m_prod_semielasticity_data, luprop9m_prod_semielasticity_data, luprop13m_prod_semielasticity_data, lupropg13m_prod_semielasticity_data
   REAL(8) :: luprop3m_unemp_elasticity_data, luprop5m_unemp_elasticity_data, luprop9m_unemp_elasticity_data, luprop13m_unemp_elasticity_data, lupropg13m_unemp_elasticity_data
   REAL(8) :: luprop3m_unemp_semielasticity_data, luprop5m_unemp_semielasticity_data, luprop9m_unemp_semielasticity_data, luprop13m_unemp_semielasticity_data, lupropg13m_unemp_semielasticity_data

   REAL(8) :: ult5wk_ave_data, u5lt15wk_ave_data, u15lt27wk_ave_data, ugt27wk_ave_data
   REAL(8) :: var_ult5wk_ave_data, var_u5lt15wk_ave_data, var_u15lt27wk_ave_data, var_ugt27wk_ave_data
   REAL(8) :: var_l_ult5wk_ave_data, var_l_u5lt15wk_ave_data, var_l_u15lt27wk_ave_data, var_l_ugt27wk_ave_data


   REAL(8) :: uprop_3m_ave_data, uprop_5m_ave_data, uprop_9m_ave_data, uprop_13m_ave_data, uprop_g13m_ave_data
   REAL(8) :: uprop_yng_3m_ave_data, uprop_yng_5m_ave_data, uprop_yng_9m_ave_data, uprop_yng_13m_ave_data, uprop_yng_g13m_ave_data
   REAL(8) :: uprop_prm_3m_ave_data, uprop_prm_5m_ave_data, uprop_prm_9m_ave_data, uprop_prm_13m_ave_data, uprop_prm_g13m_ave_data

   REAL(8) :: var_uprop_3m_ave_data, var_uprop_5m_ave_data, var_uprop_9m_ave_data, var_uprop_13m_ave_data, var_uprop_g13m_ave_data
   REAL(8) :: var_l_uprop_3m_ave_data, var_l_uprop_5m_ave_data, var_l_uprop_9m_ave_data, var_l_uprop_13m_ave_data, var_l_uprop_g13m_ave_data
   REAL(8) :: var_uprop_yng_3m_ave_data, var_uprop_yng_5m_ave_data, var_uprop_yng_9m_ave_data, var_uprop_yng_13m_ave_data, var_uprop_yng_g13m_ave_data
   REAL(8) :: var_l_uprop_yng_3m_ave_data, var_l_uprop_yng_5m_ave_data, var_l_uprop_yng_9m_ave_data, var_l_uprop_yng_13m_ave_data,var_l_uprop_yng_g13m_ave_data
   REAL(8) :: var_uprop_prm_3m_ave_data, var_uprop_prm_5m_ave_data, var_uprop_prm_9m_ave_data, var_uprop_prm_13m_ave_data, var_uprop_prm_g13m_ave_data
   REAL(8) :: var_l_uprop_prm_3m_ave_data, var_l_uprop_prm_5m_ave_data, var_l_uprop_prm_9m_ave_data, var_l_uprop_prm_13m_ave_data, var_l_uprop_prm_g13m_ave_data


   !! HPFILTERED STATS
   REAL(8) :: var_hp_uprop_3m_ave_data, var_hp_uprop_5m_ave_data, var_hp_uprop_9m_ave_data, var_hp_uprop_13m_ave_data, var_hp_uprop_g13m_ave_data
   REAL(8) :: var_hp_l_uprop_3m_ave_data, var_hp_l_uprop_5m_ave_data, var_hp_l_uprop_9m_ave_data, var_hp_l_uprop_13m_ave_data, var_hp_l_uprop_g13m_ave_data
   REAL(8) :: var_hp_uprop_yng_3m_ave_data, var_hp_uprop_yng_5m_ave_data, var_hp_uprop_yng_9m_ave_data, var_hp_uprop_yng_13m_ave_data, var_hp_uprop_yng_g13m_ave_data
   REAL(8) :: var_hp_l_uprop_yng_3m_ave_data, var_hp_l_uprop_yng_5m_ave_data, var_hp_l_uprop_yng_9m_ave_data, var_hp_l_uprop_yng_13m_ave_data, var_hp_l_uprop_yng_g13m_ave_data
   REAL(8) :: var_hp_uprop_prm_3m_ave_data, var_hp_uprop_prm_5m_ave_data, var_hp_uprop_prm_9m_ave_data, var_hp_uprop_prm_13m_ave_data, var_hp_uprop_prm_g13m_ave_data
   REAL(8) :: var_hp_l_uprop_prm_3m_ave_data, var_hp_l_uprop_prm_5m_ave_data, var_hp_l_uprop_prm_9m_ave_data, var_hp_l_uprop_prm_13m_ave_data, var_hp_l_uprop_prm_g13m_ave_data


   REAL(8) :: hp_uprop3m_prod_elasticity_data, hp_uprop5m_prod_elasticity_data, hp_uprop9m_prod_elasticity_data, hp_uprop13m_prod_elasticity_data,hp_upropg13m_prod_elasticity_data
   REAL(8) :: hp_uprop3m_prod_semielasticity_data, hp_uprop5m_prod_semielasticity_data, hp_uprop9m_prod_semielasticity_data, hp_uprop13m_prod_semielasticity_data, hp_upropg13m_prod_semielasticity_data
   REAL(8) :: hp_uprop3m_unemp_elasticity_data, hp_uprop5m_unemp_elasticity_data, hp_uprop9m_unemp_elasticity_data, hp_uprop13m_unemp_elasticity_data, hp_upropg13m_unemp_elasticity_data
   REAL(8) :: hp_uprop3m_unemp_semielasticity_data, hp_uprop5m_unemp_semielasticity_data, hp_uprop9m_unemp_semielasticity_data, hp_uprop13m_unemp_semielasticity_data, hp_upropg13m_unemp_semielasticity_data
   REAL(8) :: hp_uprop3m_unemp_semielasticity_instdata, hp_uprop5m_unemp_semielasticity_instdata, hp_uprop9m_unemp_semielasticity_instdata, hp_uprop13m_unemp_semielasticity_instdata, hp_upropg13m_unemp_semielasticity_instdata
   REAL(8) :: sm_hp_uprop3m_unemp_semielasticity_data, sm_hp_uprop5m_unemp_semielasticity_data, sm_hp_uprop9m_unemp_semielasticity_data, sm_hp_uprop13m_unemp_semielasticity_data, sm_hp_upropg13m_unemp_semielasticity_data
   REAL(8) :: hpl_uprop3m_prod_elasticity_data, hpl_uprop5m_prod_elasticity_data, hpl_uprop9m_prod_elasticity_data, hpl_uprop13m_prod_elasticity_data, hpl_upropg13m_prod_elasticity_data
   REAL(8) :: hpl_uprop3m_prod_semielasticity_data, hpl_uprop5m_prod_semielasticity_data, hpl_uprop9m_prod_semielasticity_data, hpl_uprop13m_prod_semielasticity_data, hpl_upropg13m_prod_semielasticity_data
   REAL(8) :: hpl_uprop3m_unemp_elasticity_data, hpl_uprop5m_unemp_elasticity_data, hpl_uprop9m_unemp_elasticity_data, hpl_uprop13m_unemp_elasticity_data, hpl_upropg13m_unemp_elasticity_data
   REAL(8) :: hpl_uprop3m_unemp_semielasticity_data, hpl_uprop5m_unemp_semielasticity_data, hpl_uprop9m_unemp_semielasticity_data, hpl_uprop13m_unemp_semielasticity_data, hpl_upropg13m_unemp_semielasticity_data



   REAL(8) :: var_hp_ult5wk_ave_data, var_hp_u5lt15wk_ave_data, var_hp_u15lt27wk_ave_data, var_hp_ugt27wk_ave_data
   REAL(8) :: var_hp_l_ult5wk_ave_data, var_hp_l_u5lt15wk_ave_data, var_hp_l_u15lt27wk_ave_data, var_hp_l_ugt27wk_ave_data

    REAL(8) :: hp_uprop5w_prod_elasticity_data, hp_uprop15w_prod_elasticity_data, hp_uprop27w_prod_elasticity_data, hp_upropgt27w_prod_elasticity_data
   REAL(8) :: hp_uprop5w_prod_semielasticity_data, hp_uprop15w_prod_semielasticity_data, hp_uprop27w_prod_semielasticity_data, hp_upropgt27w_prod_semielasticity_data
   REAL(8) :: hp_uprop5w_unemp_elasticity_data, hp_uprop15w_unemp_elasticity_data, hp_uprop27w_unemp_elasticity_data, hp_upropgt27w_unemp_elasticity_data
   REAL(8) :: hp_uprop5w_unemp_semielasticity_data, hp_uprop15w_unemp_semielasticity_data, hp_uprop27w_unemp_semielasticity_data, hp_upropgt27w_unemp_semielasticity_data
   REAL(8) :: hpl_uprop5w_prod_elasticity_data, hpl_uprop15w_prod_elasticity_data, hpl_uprop27w_prod_elasticity_data, hpl_upropgt27w_prod_elasticity_data
   REAL(8) :: hpl_uprop5w_prod_semielasticity_data, hpl_uprop15w_prod_semielasticity_data, hpl_uprop27w_prod_semielasticity_data, hpl_upropgt27w_prod_semielasticity_data
   REAL(8) :: hpl_uprop5w_unemp_elasticity_data, hpl_uprop15w_unemp_elasticity_data, hpl_uprop27w_unemp_elasticity_data, hpl_upropgt27w_unemp_elasticity_data
   REAL(8) :: hpl_uprop5w_unemp_semielasticity_data, hpl_uprop15w_unemp_semielasticity_data, hpl_uprop27w_unemp_semielasticity_data, hpl_upropgt27w_unemp_semielasticity_data

   REAL(8) :: u3m_unemp_yng_semielasticity_data, u5m_unemp_yng_semielasticity_data, u9m_unemp_yng_semielasticity_data, u13m_unemp_yng_semielasticity_data, ug13m_unemp_yng_semielasticity_data
   REAL(8) :: u3m_unemp_prm_semielasticity_data, u5m_unemp_prm_semielasticity_data, u9m_unemp_prm_semielasticity_data, u13m_unemp_prm_semielasticity_data, ug13m_unemp_prm_semielasticity_data
   REAL(8) :: u3m_unemp_yng_elasticity_data, u5m_unemp_yng_elasticity_data, u9m_unemp_yng_elasticity_data, u13m_unemp_yng_elasticity_data, ug13m_unemp_yng_elasticity_data
   REAL(8) :: u3m_unemp_prm_elasticity_data, u5m_unemp_prm_elasticity_data, u9m_unemp_prm_elasticity_data, u13m_unemp_prm_elasticity_data, ug13m_unemp_prm_elasticity_data

   REAL(8) :: hp_uprop3m_unemp_yng_elasticity_data, hp_uprop5m_unemp_yng_elasticity_data, hp_uprop9m_unemp_yng_elasticity_data, hp_uprop13m_unemp_yng_elasticity_data, hp_upropg13m_unemp_yng_elasticity_data
   REAL(8) :: hp_uprop3m_unemp_yng_semielasticity_data, hp_uprop5m_unemp_yng_semielasticity_data, hp_uprop9m_unemp_yng_semielasticity_data, hp_uprop13m_unemp_yng_semielasticity_data, hp_upropg13m_unemp_yng_semielasticity_data
   REAL(8) :: hp_uprop3m_unemp_yng_semielasticity_instdata, hp_uprop5m_unemp_yng_semielasticity_instdata, hp_uprop9m_unemp_yng_semielasticity_instdata, hp_uprop13m_unemp_yng_semielasticity_instdata, hp_upropg13m_unemp_yng_semielasticity_instdata

   REAL(8) :: hp_uprop3m_unemp_prm_elasticity_data, hp_uprop5m_unemp_prm_elasticity_data, hp_uprop9m_unemp_prm_elasticity_data, hp_uprop13m_unemp_prm_elasticity_data, hp_upropg13m_unemp_prm_elasticity_data
   REAL(8) :: hp_uprop3m_unemp_prm_semielasticity_data, hp_uprop5m_unemp_prm_semielasticity_data, hp_uprop9m_unemp_prm_semielasticity_data, hp_uprop13m_unemp_prm_semielasticity_data, hp_upropg13m_unemp_prm_semielasticity_data
   REAL(8) :: hp_uprop3m_unemp_prm_semielasticity_instdata, hp_uprop5m_unemp_prm_semielasticity_instdata, hp_uprop9m_unemp_prm_semielasticity_instdata, hp_uprop13m_unemp_prm_semielasticity_instdata, hp_upropg13m_unemp_prm_semielasticity_instdata


    REAL(8) :: udur_occ_ave_data, udur_nocc_ave_data, udur_dnocc_ave_data, udur_dnocc_young_ave_data, udur_dnocc_prime_ave_data
    REAL(8) :: udur_nocc_with_u_data, udur_occ_with_u_data, udur_dnocc_with_u_data, udur_dnocc_young_with_u_data, udur_dnocc_prime_with_u_data
    REAL(8) :: udur_nocc_with_hpu_data, udur_occ_with_hpu_data, udur_dnocc_with_hpu_data, udur_dnocc_young_with_hpu_data, udur_dnocc_prime_with_hpu_data
    REAL(8) :: rel_udur_move_stay_data


    REAL(8) :: m1mratio_data
    REAL(8) :: m5mratio_data

    REAL(8) :: bevcurve_data
    REAL(8) :: relstdev_occmob_y_data

    !==================
    ! UNEMPLOYMENT DISTRIBUTION EXTREMES/BOUNDS
    !=================

    ! su urate_bls ur_all_durwvcearly in  "C:\data\ts_urate_jf_sep\timeseries_u_sept2018excerpt.dta"
    REAL(8) :: min_unemp_data=0.039_8 ! MIN urate_bls
    REAL(8) :: min_unemp_ee_data=0.0188 ! MIN ur_all_durwvcearly
    !REAL(8) :: min_unemp_y_data=0.039_8 ! MIN urate_bls
    REAL(8) :: min_unemp_young_ee_data=0.029 ! MIN ur_all_durwvcearly
    !REAL(8) :: min_unemp_data=0.039_8 ! MIN urate_bls
    REAL(8) :: min_unemp_prime_ee_data=0.0152 ! MIN ur_all_durwvcearly

    REAL(8) :: max_unemp_data=0.063_8 ! MIN urate_bls
    REAL(8) :: max_unemp_ee_data=0.101    ! MIN ur_all_durwvcearly
    !REAL(8) :: min_unemp_y_data=0.039_8 ! MIN urate_bls
    REAL(8) :: max_unemp_young_ee_data=0.081 ! MIN ur_all_durwvcearly
    !REAL(8) :: min_unemp_data=0.039_8 ! MIN urate_bls
    REAL(8) :: max_unemp_prime_ee_data=0.058 ! MIN ur_all_durwvcearly


    REAL(8) :: unemp_ee_p5_data, unemp_ee_p10_data, unemp_ee_p25_data, unemp_ee_p50_data, unemp_ee_p75_data, unemp_ee_p90_data, unemp_ee_p95_data


    REAL(8) :: cycl_sep_age_shift_muellermom_data

    REAL(8) :: profileshift_shortdur_data
    REAL(8) :: profileshift_longdur_data

    REAL(8), DIMENSION(12) :: udur_mvec_occ_ave_data, udur_mvec_occ_young_ave_data, udur_mvec_occ_prime_ave_data, &
                                udur_mvec_nocc_ave_data, udur_mvec_nocc_young_ave_data, udur_mvec_nocc_prime_ave_data
    REAL(8), DIMENSION(12) :: udur_mvec_occ_with_u_data, udur_mvec_occ_young_with_u_data, udur_mvec_occ_prime_with_u_data, &
                                udur_mvec_nocc_with_u_data, udur_mvec_nocc_young_with_u_data, udur_mvec_nocc_prime_with_u_data
    REAL(8), DIMENSION(12) :: udur_mvec_occ_with_hpu_data, udur_mvec_occ_young_with_hpu_data, udur_mvec_occ_prime_with_hpu_data, &
                                udur_mvec_nocc_with_hpu_data, udur_mvec_nocc_young_with_hpu_data, udur_mvec_nocc_prime_with_hpu_data
    
   REAL(8) :: udur_occ_with_hplu_data, udur_occ_with_lu_data
   REAL(8) :: udur_occ_young_with_hplu_data, udur_occ_young_with_lu_data
   REAL(8) :: udur_occ_prime_with_hplu_data, udur_occ_prime_with_lu_data
   REAL(8) :: udur_nocc_with_hplu_data, udur_nocc_with_lu_data
   REAL(8) :: udur_nocc_young_with_hplu_data, udur_nocc_young_with_lu_data
   REAL(8) :: udur_nocc_prime_with_hplu_data, udur_nocc_prime_with_lu_data


    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p33_data, tot_durationmatrix_cocc_p67_data,tot_durationmatrix_cr_cocc_p33_data, tot_durationmatrix_cr_cocc_p67_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p50_data, tot_durationmatrix_cr_cocc_p50_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_nocc_p33_data, tot_durationmatrix_nocc_p67_data,tot_durationmatrix_cr_nocc_p33_data, tot_durationmatrix_cr_nocc_p67_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_nocc_p50_data, tot_durationmatrix_cr_nocc_p50_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p33_lb_data, tot_durationmatrix_cocc_p67_lb_data,tot_durationmatrix_cr_cocc_p33_lb_data, tot_durationmatrix_cr_cocc_p67_lb_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p50_lb_data, tot_durationmatrix_cr_cocc_p50_lb_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p33_ub_data, tot_durationmatrix_cocc_p67_ub_data,tot_durationmatrix_cr_cocc_p33_ub_data, tot_durationmatrix_cr_cocc_p67_ub_data
    REAL(8), DIMENSION(24) :: tot_durationmatrix_cocc_p50_ub_data, tot_durationmatrix_cr_cocc_p50_ub_data
    
    REAL(8) :: occmob_3m_p33_data, occmob_3m_p67_data, occmob_6m_p33_data, occmob_6m12_p67_data
    REAL(8) :: logwage_disp_diff_all_data, logwage_disp_diff_highoccten_data,  logwage_disp_uhires_data

    !!================================
    !!  NET MOBILITY DATA MOMENTS
    !!================================

    REAL(8), DIMENSION(4) :: corr_mogmob_superocc_data, mogmob_superocc_data, ave_occdistr_data, corr_ave_occdistr_data           ! from MOG flow matrix (source: )
    REAL(8), DIMENSION(4,2) :: corr_occdistr_begin_end_data, occdistr_begin_end_data       ! check from data

    !! SUPOCC FLOW MATRICES. FROM DATA
    REAL(8), DIMENSION(4, 4) :: data_superocc_transmatrix_data, data_corr_superocc_transmatrix_data, data_superocc_transmatrix_forreal_data, data_corr_superocc_transmatrix_forreal_data
    REAL(8), DIMENSION(4, 4) :: data_superocc_transmatrix_p33_data, data_corr_superocc_transmatrix_p33_data
    REAL(8), DIMENSION(4, 4) :: data_superocc_transmatrix_p50_data, data_corr_superocc_transmatrix_p50_data
    REAL(8), DIMENSION(4, 4) :: data_superocc_transmatrix_p67_data, data_corr_superocc_transmatrix_p67_data

    !! DERIVED FROM SUPOCC FLOW MATRICES
    REAL(8), DIMENSION(4) :: corr_netmob_calc_vector_data, netmob_calc_vector_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_netmob_calc_vector_p33_data, netmob_calc_vector_p33_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_netmob_calc_vector_p67_data, netmob_calc_vector_p67_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_netmob_calc_vector_p50_data, netmob_calc_vector_p50_data         ! calculated from SUPEROCC flow matrix, not put in manually

    REAL(8), DIMENSION(4) :: corr_supocc_outflow_data, supocc_outflow_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_outflow_p33_data, supocc_outflow_p33_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_outflow_p67_data, supocc_outflow_p67_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_outflow_p50_data, supocc_outflow_p50_data         ! calculated from SUPEROCC flow matrix, not put in manually

    REAL(8), DIMENSION(4) :: corr_supocc_inflow_data, supocc_inflow_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_inflow_p33_data, supocc_inflow_p33_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_inflow_p67_data, supocc_inflow_p67_data         ! calculated from SUPEROCC flow matrix, not put in manually
    REAL(8), DIMENSION(4) :: corr_supocc_inflow_p50_data, supocc_inflow_p50_data         ! calculated from SUPEROCC flow matrix, not put in manually

    !! FURTHER SUMMARY OF MOBILITY
    REAL(8), DIMENSION(4)         :: grossmob_calc_vector_data, corr_grossmob_calc_vector_data
    REAL(8)                            :: overall_netmob_data, overall_grossmob_data, overall_xsmob_data
    REAL(8)                            :: overall_corr_netmob_data, overall_corr_grossmob_data, overall_corr_xsmob_data
    REAL(8), DIMENSION(4)         :: grossmob_calc_vector_p33_data, corr_grossmob_calc_vector_p33_data
    REAL(8)                            :: overall_netmob_p33_data, overall_grossmob_p33_data, overall_xsmob_p33_data
    REAL(8)                            :: overall_corr_netmob_p33_data, overall_corr_grossmob_p33_data, overall_corr_xsmob_p33_data
    REAL(8), DIMENSION(4)         :: grossmob_calc_vector_p50_data, corr_grossmob_calc_vector_p50_data
    REAL(8)                            :: overall_netmob_p50_data, overall_grossmob_p50_data, overall_xsmob_p50_data
    REAL(8)                            :: overall_corr_netmob_p50_data, overall_corr_grossmob_p50_data, overall_corr_xsmob_p50_data
    REAL(8), DIMENSION(4)         :: grossmob_calc_vector_p67_data, corr_grossmob_calc_vector_p67_data
    REAL(8)                            :: overall_netmob_p67_data, overall_grossmob_p67_data, overall_xsmob_p67_data
    REAL(8)                            :: overall_corr_netmob_p67_data, overall_corr_grossmob_p67_data, overall_corr_xsmob_p67_data

    REAL(8), DIMENSION(4) :: udur_supocc_unemp_elasticity_ldt_data, udur_supocc_unemp_semielasticity_ldt_data
    REAL(8), DIMENSION(4) :: sep_supocc_unemp_elasticity_ldt_data, sep_supocc_unemp_semielasticity_ldt_data
    REAL(8)               :: sep_unemp_elasticity_ldt_data, sep_unemp_semielasticity_ldt_data

    INTEGER :: acnt

CONTAINS


    SUBROUTINE set_auxdata_moments


!===========================
!   OCCUPATIONAL DISTRIBUTIONS -- SET EXTERNALLY IN PROGRAM
!==============================

    
    !! CORRECTED - 1984
    corr_occdistr_begin_end_data(1,1)=0.223604
    corr_occdistr_begin_end_data(2,1)=0.292432
    corr_occdistr_begin_end_data(3,1)=0.226258
    corr_occdistr_begin_end_data(4,1)=0.257706

    !! uncorrected
    occdistr_begin_end_data(1,1)=0.222405859
    occdistr_begin_end_data(2,1)=0.29835691
    occdistr_begin_end_data(3,1)=0.226892966
    occdistr_begin_end_data(4,1)=0.252344266


    !! corrected end 2012
    corr_occdistr_begin_end_data(1,2)=0.328523      !0.32921 !(2013)
    corr_occdistr_begin_end_data(2,2)=0.25789       !0.257226
    corr_occdistr_begin_end_data(3,2)=0.259575      !0.261123
    corr_occdistr_begin_end_data(4,2)=0.154013      !0.15244

    occdistr_begin_end_data(1,2)=0.31591137
    occdistr_begin_end_data(2,2)=0.265643954
    occdistr_begin_end_data(3,2)=0.262194348
    occdistr_begin_end_data(4,2)=0.156250328

    !! AVERAGE COMPOSITION - CORRECTED 
    ave_occdistr_data(1)=0.279413    !0.272806568
    ave_occdistr_data(2)=0.278768032 !0.286793036
    ave_occdistr_data(3)=0.235933484 !0.236562475
    ave_occdistr_data(4)=0.205885484 !0.203837921

    
    
    
    
    !! INITIAL DISTRIBUTION:CDF!!!!

    init_occdistr(1)=corr_occdistr_begin_end_data(1,1)
    init_occdistr(2)=init_occdistr(1)+corr_occdistr_begin_end_data(2,1)
    init_occdistr(3)=init_occdistr(2)+corr_occdistr_begin_end_data(3,1)
    init_occdistr(4)=init_occdistr(3)+corr_occdistr_begin_end_data(4,1)


!!========================================
!!  MISCODING
!!========================================
    
!!!  MISCODING DERIVED IN sep_statistics_sipp_aug2019_part1_ss.do
!!    miscode_rtnm[4,4]
!!           c1         c2         c3         c4
!!r1  .95516784  .02938673  .01161402  .01135193
!!r2  .02268214  .94562743  .01904754   .0078874
!!r3   .0136611  .02487647  .94061967  .02852219
!!r4  .01144315   .0084645  .02623058  .94190322
!
!
!
!    ! gamma miscoding
!    gamma_miscode(1,1)=0.95516784
!    gamma_miscode(1,2)=gamma_miscode(1,1)+0.02938673
!    gamma_miscode(1,3)=gamma_miscode(1,2)+0.01161402
!    gamma_miscode(1,4)=1.0
!
!    gamma_miscode(2,1)=0.02268214
!    gamma_miscode(2,2)=gamma_miscode(2,1)+0.94562743
!    gamma_miscode(2,3)=gamma_miscode(2,2)+0.01904754
!    gamma_miscode(2,4)=1.0
!
!    gamma_miscode(3,1)=0.0136611
!    gamma_miscode(3,2)=gamma_miscode(3,1)+0.02487647
!    gamma_miscode(3,3)=gamma_miscode(3,2)+0.94061967
!    gamma_miscode(3,4)=1.0
!
!    gamma_miscode(4,1)=0.01144315
!    gamma_miscode(4,2)=gamma_miscode(4,1)+0.0084645
!    gamma_miscode(4,3)=gamma_miscode(4,2)+0.02623058
!    gamma_miscode(4,4)=1.0


    ! miscoding within supergroup, single parameter!
    ! mogmiscoding_flows_for_CALIBRATION.xls
    !gamma_misc_within_superocc=0.02534788
    ! C:\data\mogmiscoding_flows_for_CALIBRATION_feb2020.xlsx
    !!gamma_misc_within_superocc=0.032511

!!  MISCODING DERIVED IN sep_statistics_sipp_aug2019_part1_ss.do
!    miscode_rtnm[4,4]
!           c1         c2         c3         c4
!r1  .95516784  .02938673  .01161402  .01135193
!r2  .02268214  .94562743  .01904754   .0078874
!r3   .0136611  .02487647  .94061967  .02852219
!r4  .01144315   .0084645  .02623058  .94190322

!!MISCODING INCL NONMGT INTO MGT					
!!                                                              total miscoding					
!!0.117947213	0.003896357	0.00111948	0.001521432	0.124484483	0.059457744
!!0.010838744	0.231179329	0.004683524	0.001921474	0.24862307	
!!0.005086608	0.007447595	0.267397978	0.008825997	0.288758178	
!!0.005240061	0.001869653	0.007006819	0.241673081	0.255789614	
!!					
!!0.947485262	0.031299945	0.00899293	0.012221863	1	
!!0.043595084	0.929838604	0.018837851	0.00772846	1	
!!0.017615461	0.025791805	0.926027378	0.030565356	1	
!!0.020485822	0.007309339	0.0273929	0.944811939	1	

    !USE THE MEASURE THAT DOES NOT INCL. ANY MGT
!0.913568798	0.030832022	0.008858489	0.01203915
!0.021198668	0.94593295	0.019163911	0.00786223
!0.010930161	0.026578827	0.954284556	0.03149804
!0.010550095	0.00736252	0.027592205	0.951686199

!! USED IN WOLFRAM ALPHA INVERTER
!(0.913569 | 0.030832 | 0.00885849 | 0.0120392!
!0.0211987 | 0.945933 | 0.0191639 | 0.00786223
!0.0109302 | 0.0265788 | 0.954285 | 0.031498
!0.0105501 | 0.00736252 | 0.0275922 | 0.951686)

!! INVERSE OUT:
!(1.09569 | -0.0353549 | -0.0090775 | -0.0132683
!-0.0242253 | 1.05859 | -0.0208096 | -0.00775022
!-0.0114913 | -0.0288493 | 1.04958 | -0.0343545
!-0.0116259 | -0.00696121 | -0.030169 | 1.05197)    
    ! gamma miscoding
    gamma_miscode(1,1)=0.91344788
    gamma_miscode(1,2)=gamma_miscode(1,1)+0.03081548
    gamma_miscode(1,3)=gamma_miscode(1,2)+0.00891258 
    gamma_miscode(1,4)=1.0

    gamma_miscode(2,1)=0.02121608
    gamma_miscode(2,2)=gamma_miscode(2,1)+0.94619679  
    gamma_miscode(2,3)=gamma_miscode(2,2)+0.01908765 
    gamma_miscode(2,4)=1.0

    gamma_miscode(3,1)=0.01089613   
    gamma_miscode(3,2)=gamma_miscode(3,1)+0.0264022  
    gamma_miscode(3,3)=gamma_miscode(3,2)+0.95434151	
    gamma_miscode(3,4)=1.0

    gamma_miscode(4,1)=0.01063076  	
    gamma_miscode(4,2)=gamma_miscode(4,1)+0.00732213  
    gamma_miscode(4,3)=gamma_miscode(4,2)+0.02757589	
    gamma_miscode(4,4)=1.0


    ! miscoding within supergroup, single parameter!
    ! mogmiscoding_flows_for_CALIBRATION.xls
    !gamma_misc_within_superocc=0.02534788
    ! C:\data\mogmiscoding_flows_for_CALIBRATION_feb2020.xlsx
    !gamma_misc_within_superocc_data=0.03153
    gamma_misc_within_superocc_data=0.02965

!! INVERSE GAMMA MISCODE
!! INVERSE OUT:
!(1.09569 | -0.0353549 | -0.0090775 | -0.0132683
!-0.0242253 | 1.05859 | -0.0208096 | -0.00775022
!-0.0114913 | -0.0288493 | 1.04958 | -0.0343545
!-0.0116259 | -0.00696121 | -0.030169 | 1.05197)    
    ! gamma miscoding
    
    
    invgamma_miscode(1,1)=  1.0958369  
    invgamma_miscode(1,2)= -0.03533137  
    invgamma_miscode(1,3)= -0.00914512  
    invgamma_miscode(1,4)= -0.01322791

    invgamma_miscode(2,1)= -0.0242441   
    invgamma_miscode(2,2)=  1.0582895    
    invgamma_miscode(2,3)= -0.0207194   
    invgamma_miscode(2,4)= -0.00764435

    invgamma_miscode(3,1)= -0.01145506  
    invgamma_miscode(3,2)= -0.02864686   
    invgamma_miscode(3,3)=  1.0495129   
    invgamma_miscode(3,4)= -0.0342639 

    invgamma_miscode(4,1)= -0.0117202  
    invgamma_miscode(4,2)= -0.00691621  
    invgamma_miscode(4,3)= -0.03014293   
    invgamma_miscode(4,4)=  1.0517567
    


!======================================================================
!   OCCUPATIONAL DISTRIBUTIONS -- FOR MATCHING INDIRECTLY OR VALIDATING
!======================================================================




    !!=========================================================
    !! SUPOCC FLOW MATRICES. FROM DATA
    !!=========================================================
    
    !!--- uncorrected
    

    data_superocc_transmatrix_data(1,1)=0.084047165	
    data_superocc_transmatrix_data(1,2)=0.026361143	
    data_superocc_transmatrix_data(1,3)=0.014780028	
    data_superocc_transmatrix_data(1,4)=0.00794079
    
    data_superocc_transmatrix_data(2,1)=0.024070865	
    data_superocc_transmatrix_data(2,2)=0.153388538	
    data_superocc_transmatrix_data(2,3)=0.047064746	
    data_superocc_transmatrix_data(2,4)=0.021301638
    
    data_superocc_transmatrix_data(3,1)=0.010385007	
    data_superocc_transmatrix_data(3,2)=0.041646338	
    data_superocc_transmatrix_data(3,3)=0.195965665	
    data_superocc_transmatrix_data(3,4)=0.056086807
    
    data_superocc_transmatrix_data(4,1)=0.008737713	
    data_superocc_transmatrix_data(4,2)=0.024585537	
    data_superocc_transmatrix_data(4,3)=0.066621694	
    data_superocc_transmatrix_data(4,4)=0.217016325
    
        !! corrected
    !! FLOW MATRIX NOT TRANSMATRIX
    
    


    data_corr_superocc_transmatrix_data(1,1)=0.092404911
    data_corr_superocc_transmatrix_data(1,2)=0.021370651	
    data_corr_superocc_transmatrix_data(1,3)=0.010757779	
    data_corr_superocc_transmatrix_data(1,4)=0.003547204

    data_corr_superocc_transmatrix_data(2,1)=0.019251603	
    data_corr_superocc_transmatrix_data(2,2)=0.167154002	
    data_corr_superocc_transmatrix_data(2,3)=0.041148107	
    data_corr_superocc_transmatrix_data(2,4)=0.018015871

    data_corr_superocc_transmatrix_data(3,1)=0.00618074	
    data_corr_superocc_transmatrix_data(3,2)=0.035725196	
    data_corr_superocc_transmatrix_data(3,3)=0.219930117	
    data_corr_superocc_transmatrix_data(3,4)=0.047982546

    data_corr_superocc_transmatrix_data(4,1)=0.004244874	
    data_corr_superocc_transmatrix_data(4,2)=0.020827353	
    data_corr_superocc_transmatrix_data(4,3)=0.05938962	
    data_corr_superocc_transmatrix_data(4,4)=0.232069429


    !! SUPEROCC 1 CHANGERS DIRECTION
                   tempreal=sum(data_corr_superocc_transmatrix_data(1,:))
                   data_corr_superocc_transmatrix_forreal_data(1,1)=data_corr_superocc_transmatrix_data(1,1)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(1,2)=data_corr_superocc_transmatrix_data(1,2)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(1,3)=data_corr_superocc_transmatrix_data(1,3)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(1,4)=data_corr_superocc_transmatrix_data(1,4)/tempreal

    !! SUPEROCC 2 CHANGERS DIRECTION
                   tempreal=sum(data_corr_superocc_transmatrix_data(2,:))
                   data_corr_superocc_transmatrix_forreal_data(2,1)=data_corr_superocc_transmatrix_data(2,1)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(2,2)=data_corr_superocc_transmatrix_data(2,2)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(2,3)=data_corr_superocc_transmatrix_data(2,3)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(2,4)=data_corr_superocc_transmatrix_data(2,4)/tempreal

    !! SUPEROCC 3 CHANGERS DIRECTION
                   tempreal=sum(data_corr_superocc_transmatrix_data(3,:))
                   data_corr_superocc_transmatrix_forreal_data(3,1)=data_corr_superocc_transmatrix_data(3,1)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(3,2)=data_corr_superocc_transmatrix_data(3,2)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(3,3)=data_corr_superocc_transmatrix_data(3,3)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(3,4)=data_corr_superocc_transmatrix_data(3,4)/tempreal

    !! SUPEROCC 4 CHANGERS DIRECTION
                   tempreal=sum(data_corr_superocc_transmatrix_data(4,:))
                   data_corr_superocc_transmatrix_forreal_data(4,1)=data_corr_superocc_transmatrix_data(4,1)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(4,2)=data_corr_superocc_transmatrix_data(4,2)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(4,3)=data_corr_superocc_transmatrix_data(4,3)/tempreal
                   data_corr_superocc_transmatrix_forreal_data(4,4)=data_corr_superocc_transmatrix_data(4,4)/tempreal


    
    
    !!===========
    !!  GOOD TIMES P33
    !!============
    ! GOOD TIMES FLOW MATRIX net_gross_picture_oct2019transmat_forcalibration.xlsx
    !!C:\Users\lviss\Dropbox\CTV_Revisions\section2_data\xls\net_gross_picture_oct2019transmat_forcalibration 20191117.xlsx

    !! UNCORRECTED 
    !! GOOD TIMES p33 (USE CORRECTED DATA FOR NOW)
    data_superocc_transmatrix_p33_data(1,1)=0.078176008	
    data_superocc_transmatrix_p33_data(1,2)=0.030856055	
    data_superocc_transmatrix_p33_data(1,3)=0.010956197	
    data_superocc_transmatrix_p33_data(1,4)=0.009707184

    data_superocc_transmatrix_p33_data(2,1)=0.027727678	
    data_superocc_transmatrix_p33_data(2,2)=0.180470213	
    data_superocc_transmatrix_p33_data(2,3)=0.050237043	
    data_superocc_transmatrix_p33_data(2,4)=0.028360266

    data_superocc_transmatrix_p33_data(3,1)=0.015118951	
    data_superocc_transmatrix_p33_data(3,2)=0.070686591	
    data_superocc_transmatrix_p33_data(3,3)=0.305846529	
    data_superocc_transmatrix_p33_data(3,4)=0.092167116

    data_superocc_transmatrix_p33_data(4,1)=0.030293032	
    data_superocc_transmatrix_p33_data(4,2)=0.075063115	
    data_superocc_transmatrix_p33_data(4,3)=0.20979293	
    data_superocc_transmatrix_p33_data(4,4)=0.684517857

    !! CORRECTED
    !! corrected
    data_corr_superocc_transmatrix_p33_data(1,1)=0.085153196
    data_corr_superocc_transmatrix_p33_data(1,2)=0.027587539	
    data_corr_superocc_transmatrix_p33_data(1,3)=0.006472873
    data_corr_superocc_transmatrix_p33_data(1,4)=0.005551023

    data_corr_superocc_transmatrix_p33_data(2,1)=0.018980546
    data_corr_superocc_transmatrix_p33_data(2,2)=0.172023815
    data_corr_superocc_transmatrix_p33_data(2,3)=0.037027683
    data_corr_superocc_transmatrix_p33_data(2,4)=0.020569993

    data_corr_superocc_transmatrix_p33_data(3,1)=0.005048349
    data_corr_superocc_transmatrix_p33_data(3,2)=0.039112056	
    data_corr_superocc_transmatrix_p33_data(3,3)=0.212234077	
    data_corr_superocc_transmatrix_p33_data(3,4)=0.050117972

    data_corr_superocc_transmatrix_p33_data(4,1)=0.005042398	
    data_corr_superocc_transmatrix_p33_data(4,2)=0.020264363	
    data_corr_superocc_transmatrix_p33_data(4,3)=0.060488623	
    data_corr_superocc_transmatrix_p33_data(4,4)=0.234325494

    !!------------------
    !! GOOD TIMES p50
    !!----------------------


    data_superocc_transmatrix_p50_data(1,1)=0.078176008	
    data_superocc_transmatrix_p50_data(1,2)=0.030856055	
    data_superocc_transmatrix_p50_data(1,3)=0.010956197	
    data_superocc_transmatrix_p50_data(1,4)=0.009707184

    data_superocc_transmatrix_p50_data(2,1)=0.024131525	
    data_superocc_transmatrix_p50_data(2,2)=0.157064049	
    data_superocc_transmatrix_p50_data(2,3)=0.043721527	
    data_superocc_transmatrix_p50_data(2,4)=0.024682069

    data_superocc_transmatrix_p50_data(3,1)=0.009384917	
    data_superocc_transmatrix_p50_data(3,2)=0.043877897	
    data_superocc_transmatrix_p50_data(3,3)=0.189850754	
    data_superocc_transmatrix_p50_data(3,4)=0.057211689

    data_superocc_transmatrix_p50_data(4,1)=0.009708519	
    data_superocc_transmatrix_p50_data(4,2)=0.024056743	
    data_superocc_transmatrix_p50_data(4,3)=0.06723588	
    data_superocc_transmatrix_p50_data(4,4)=0.219378988

    !! corrected
    !! GOOD TIMES FLOW MATRIX 
    
    data_corr_superocc_transmatrix_p50_data(1,1)=0.085403687
    data_corr_superocc_transmatrix_p50_data(1,2)=0.02160707	
    data_corr_superocc_transmatrix_p50_data(1,3)=0.005853685	
    data_corr_superocc_transmatrix_p50_data(1,4)=0.004390252

    data_corr_superocc_transmatrix_p50_data(2,1)=0.019656502	
    data_corr_superocc_transmatrix_p50_data(2,2)=0.168329614	
    data_corr_superocc_transmatrix_p50_data(2,3)=0.035175436	
    data_corr_superocc_transmatrix_p50_data(2,4)=0.02353519

    data_corr_superocc_transmatrix_p50_data(3,1)=0.005268807	
    data_corr_superocc_transmatrix_p50_data(3,2)=0.035773324	
    data_corr_superocc_transmatrix_p50_data(3,3)=0.219135753	
    data_corr_superocc_transmatrix_p50_data(3,4)=0.052142513

    data_corr_superocc_transmatrix_p50_data(4,1)=0.004451929	
    data_corr_superocc_transmatrix_p50_data(4,2)=0.020352088	
    data_corr_superocc_transmatrix_p50_data(4,3)=0.063244016	
    data_corr_superocc_transmatrix_p50_data(4,4)=0.235680134




     !!-----------------   
     !! BAD TIMES p67
     !!-----------------
    
    data_superocc_transmatrix_p67_data(1,1)=0.093830286	
    data_superocc_transmatrix_p67_data(1,2)=0.027838792	
    data_superocc_transmatrix_p67_data(1,3)=0.018906193	
    data_superocc_transmatrix_p67_data(1,4)=0.00866693

    data_superocc_transmatrix_p67_data(2,1)=0.024817826	
    data_superocc_transmatrix_p67_data(2,2)=0.148916335	
    data_superocc_transmatrix_p67_data(2,3)=0.049918491	
    data_superocc_transmatrix_p67_data(2,4)=0.018530728

    data_superocc_transmatrix_p67_data(3,1)=0.012320006		
    data_superocc_transmatrix_p67_data(3,2)=0.039975606	
    data_superocc_transmatrix_p67_data(3,3)=0.19051032	
    data_superocc_transmatrix_p67_data(3,4)=0.050805301

    data_superocc_transmatrix_p67_data(4,1)=0.007660707	
    data_superocc_transmatrix_p67_data(4,2)=0.022379701	
    data_superocc_transmatrix_p67_data(4,3)=0.063387734	
    data_superocc_transmatrix_p67_data(4,4)=0.221535045

    
    !! CORRECTED P67 BAD
    !! corrected
    data_corr_superocc_transmatrix_p67_data(1,1)=0.104049597	
    data_corr_superocc_transmatrix_p67_data(1,2)=0.022307525	
    data_corr_superocc_transmatrix_p67_data(1,3)=0.015381501	
    data_corr_superocc_transmatrix_p67_data(1,4)=0.0042386

    data_corr_superocc_transmatrix_p67_data(2,1)=0.020030016	
    data_corr_superocc_transmatrix_p67_data(2,2)=0.162270547	
    data_corr_superocc_transmatrix_p67_data(2,3)=0.044696266	
    data_corr_superocc_transmatrix_p67_data(2,4)=0.014941384

    data_corr_superocc_transmatrix_p67_data(3,1)=0.008260673	
    data_corr_superocc_transmatrix_p67_data(3,2)=0.034014822	
    data_corr_superocc_transmatrix_p67_data(3,3)=0.213852266	
    data_corr_superocc_transmatrix_p67_data(3,4)=0.041304205

    data_corr_superocc_transmatrix_p67_data(4,1)=0.002880338	
    data_corr_superocc_transmatrix_p67_data(4,2)=0.018089202	
    data_corr_superocc_transmatrix_p67_data(4,3)=0.056191078	
    data_corr_superocc_transmatrix_p67_data(4,4)=0.237491979

    
    
    
    

    !=============================
    !  SUPEROCC STATISTICS
    !============================

                    !! C:\Users\lviss\Dropbox\CTV_Revisions\section2_data\xls\net_gross_picture_oct2019transmat_forcalibration 20200213.xlsx!
                    !! table2_appxtable2_superocc_udur_elasticity_data.txt
                     
    
    
                    !! LOG LURATE DURWVC EARLY								
	                udur_supocc_unemp_elasticity_ldt_data(1)=0.409815
                    udur_supocc_unemp_elasticity_ldt_data(2)=0.3836653
                    udur_supocc_unemp_elasticity_ldt_data(3)=0.2838073
                    udur_supocc_unemp_elasticity_ldt_data(4)=0.4183958


                    udur_supocc_unemp_semielasticity_ldt_data(1)=1.852157
                    udur_supocc_unemp_semielasticity_ldt_data(2)=1.85292	
                    udur_supocc_unemp_semielasticity_ldt_data(3)=1.367563	
                    udur_supocc_unemp_semielasticity_ldt_data(4)=1.961506	

                    !!-------------
                    !! SEPARATIONS
                    !!-----------------


!=======================
!  SEPARATIONS DATA
!============================
                      
                      
                      
                      !table2_appxtable4_separation_uconcentration.txt
                      sepyoung_ave_data=0.0089380311010304_8 !! USED IN CALIBRATION ONLY AS RELATIVE RATIO
                      sepprime_ave_data=0.0044815192046843_8 !! USED IN CALIBRATION ONLY AS RELATIVE RATIO
                        
                      sep_ave_12m_data=0.0600 
                      sep_ave_12m_posthire_data=0.2968
                      
                      sepocc_ave_data=0.0240325                     ! ave. monthly sep hazard in the first 12 months of the relationship, after an occ move  
                      sepnocc_ave_data=0.0264427                    ! ave. monthly sep hazard in the first 12 months of the relationship, after an occ stay
                      
                      

 !==========================================
 !  SURVIVAL AND OCCMOB DURATION PROFILE
 !==========================================


                     !=================================
                     ! BASIC FLOW DATA
                    
                    !table2_mean_unemp.txt
                    u_data=0.03546_8              ! unemployment rate
                    u_ave_data=0.03546_8
                      
                    ! e.g. appxtable4_survprofile_xs_calibration.txt (which uses the data directly calculated from the SIPP, not the data filled in here)
                    tot_durationmatrix_data(0)=1.0_8
                    tot_durationmatrix_data(1)=1.0_8
                    tot_durationmatrix_data(2)=0.758
                    tot_durationmatrix_data(3)=0.590
                    tot_durationmatrix_data(4)=0.457
                    tot_durationmatrix_data(5)=0.357
                    tot_durationmatrix_data(6)=0.293
                    tot_durationmatrix_data(7)=0.247
                    tot_durationmatrix_data(8)=0.208
                    tot_durationmatrix_data(9)=0.178
                    tot_durationmatrix_data(10)=0.154
                    tot_durationmatrix_data(11)=0.139
                    tot_durationmatrix_data(12)=0.120
                    tot_durationmatrix_data(13)=0.104
                    tot_durationmatrix_data(14)=0.093
                    tot_durationmatrix_data(15)=0.081
                    tot_durationmatrix_data(16)=0.076
                    tot_durationmatrix_data(17)=0.069	
                    tot_durationmatrix_data(18)=0.063
                    tot_durationmatrix_data(19)=0.057
                    tot_durationmatrix_data(20)=0.048
                    tot_durationmatrix_data(21)=0.0
                    tot_durationmatrix_data(22)=0.0
                    tot_durationmatrix_data(23)=0.0
                    tot_durationmatrix_data(24)=0.0

                    ! YOUNG
                    tot_durationmatrix_young_data(0)=1.0_8
                    tot_durationmatrix_young_data(1)=1.0_8
                    tot_durationmatrix_young_data(2)=0.697
                    tot_durationmatrix_young_data(3)=0.508
                    tot_durationmatrix_young_data(4)=0.381
                    tot_durationmatrix_young_data(5)=0.286
                    tot_durationmatrix_young_data(6)=0.223
                    tot_durationmatrix_young_data(7)=0.194
                    tot_durationmatrix_young_data(8)=0.156
                    tot_durationmatrix_young_data(9)=0.127	
                    tot_durationmatrix_young_data(10)=0.109
                    tot_durationmatrix_young_data(11)=0.095
                    tot_durationmatrix_young_data(12)=0.073
                    tot_durationmatrix_young_data(13)=0.054
                    tot_durationmatrix_young_data(14)=0.047
                    tot_durationmatrix_young_data(15)=0.041
                    tot_durationmatrix_young_data(16)=0.038
                    tot_durationmatrix_young_data(17)=0.033
                    tot_durationmatrix_young_data(18)=0.031
                    tot_durationmatrix_young_data(19)=0.0246
                    tot_durationmatrix_young_data(20)=0.0202
                    tot_durationmatrix_young_data(21)=0.0
                    tot_durationmatrix_young_data(22)=0.0
                    tot_durationmatrix_young_data(23)=0.0
                    tot_durationmatrix_young_data(24)=0.0



                    ! PRIME 
                    tot_durationmatrix_prime_data(0)=1.0_8
                    tot_durationmatrix_prime_data(1)=1.0_8
                    tot_durationmatrix_prime_data(2)=0.777
                    tot_durationmatrix_prime_data(3)=0.613	
                    tot_durationmatrix_prime_data(4)=0.485	
                    tot_durationmatrix_prime_data(5)=0.379	
                    tot_durationmatrix_prime_data(6)=0.326
                    tot_durationmatrix_prime_data(7)=0.270
                    tot_durationmatrix_prime_data(8)=0.234
                    tot_durationmatrix_prime_data(9)=0.203
                    tot_durationmatrix_prime_data(10)=0.174
                    tot_durationmatrix_prime_data(11)=0.156
                    tot_durationmatrix_prime_data(12)=0.137
                    tot_durationmatrix_prime_data(13)=0.125
                    tot_durationmatrix_prime_data(14)=0.110
                    tot_durationmatrix_prime_data(15)=0.097
                    tot_durationmatrix_prime_data(16)=0.090
                    tot_durationmatrix_prime_data(17)=0.083
                    tot_durationmatrix_prime_data(18)=0.074
                    tot_durationmatrix_prime_data(19)=0.068
                    tot_durationmatrix_prime_data(20)=0.061
                    tot_durationmatrix_prime_data(21)=0.0
                    tot_durationmatrix_prime_data(22)=0.0
                    tot_durationmatrix_prime_data(22)=0.0
                    tot_durationmatrix_prime_data(22)=0.0


                    !! ex-post mover hazard
                     tot_udurationmatrix_m_data(0)=1.0_8
                     tot_udurationmatrix_m_data(1)=0.7672 
                     tot_udurationmatrix_m_data(2)=0.6058 
                     tot_udurationmatrix_m_data(3)=0.4838 
                     tot_udurationmatrix_m_data(4)=0.3739 
                     tot_udurationmatrix_m_data(5)=0.3174 
                     tot_udurationmatrix_m_data(6)=0.2737 
                     tot_udurationmatrix_m_data(7)=0.2298 
                     tot_udurationmatrix_m_data(8)=0.2022 
                     tot_udurationmatrix_m_data(9)=0.1724 
                     tot_udurationmatrix_m_data(10)=0.1562 
                     tot_udurationmatrix_m_data(11)=0.1349 
                     tot_udurationmatrix_m_data(12)=0.1162 
                     tot_udurationmatrix_m_data(13)=0.1043 
                     tot_udurationmatrix_m_data(14)=0.0890 
                     tot_udurationmatrix_m_data(15)=0.0842 
                     tot_udurationmatrix_m_data(16)=0.0761 
                     tot_udurationmatrix_m_data(17)=0.0705 
                     tot_udurationmatrix_m_data(18)=0.0615 
                     tot_udurationmatrix_m_data(19)=0.0532 


                    !! ex-post stayer hazard
                     tot_udurationmatrix_s_data(0)=1.0_8
                     tot_udurationmatrix_s_data(1)=0.7532 
                     tot_udurationmatrix_s_data(2)=0.5716 
                     tot_udurationmatrix_s_data(3)=0.4288 
                     tot_udurationmatrix_s_data(4)=0.3415 
                     tot_udurationmatrix_s_data(5)=0.2697 
                     tot_udurationmatrix_s_data(6)=0.2215 
                     tot_udurationmatrix_s_data(7)=0.1890 
                     tot_udurationmatrix_s_data(8)=0.1562 
                     tot_udurationmatrix_s_data(9)=0.1378 
                     tot_udurationmatrix_s_data(10)=0.1240 
                     tot_udurationmatrix_s_data(11)=0.1076 
                     tot_udurationmatrix_s_data(12)=0.0960 
                     tot_udurationmatrix_s_data(13)=0.0843 
                     tot_udurationmatrix_s_data(14)=0.0744 
                     tot_udurationmatrix_s_data(15)=0.0696 
                     tot_udurationmatrix_s_data(16)=0.0630 
                     tot_udurationmatrix_s_data(17)=0.0560 
                     tot_udurationmatrix_s_data(18)=0.0522 
                     tot_udurationmatrix_s_data(19)=0.0439 




                    != === === division of unemployment spells within three-year window, across individuals
                    uproportion_window_data=0.1222  !table2_appxtable4_separation_uconcentration.txt
                    uproportion_window_y_data=0.1861        ! not double checked in most recent version of data
                    uproportion_window_p_data=0.1115        ! not double checked  in most recent version of data            
                    

                    !! CALCULATE HAZARDS  (changed)

                    DO acnt=1, 18
                        tot_haz_durationmatrix_data(acnt)=1.0-(tot_durationmatrix_data(acnt+1)/tot_durationmatrix_data(acnt))
                        tot_haz_durationmatrix_young_data(acnt)=1.0-(tot_durationmatrix_young_data(acnt+1)/tot_durationmatrix_young_data(acnt))
                        tot_haz_durationmatrix_prime_data(acnt)=1.0-(tot_durationmatrix_prime_data(acnt+1)/tot_durationmatrix_prime_data(acnt))
                        tot_haz_udurationmatrix_m_data(acnt)=1.0-(tot_udurationmatrix_m_data(acnt)/tot_udurationmatrix_m_data(acnt-1))    
                        tot_haz_udurationmatrix_m_young_data(acnt)=1.0-(tot_udurationmatrix_m_young_data(acnt)/tot_udurationmatrix_m_young_data(acnt-1))
                        tot_haz_udurationmatrix_m_prime_data(acnt)=1.0-(tot_udurationmatrix_m_prime_data(acnt)/tot_udurationmatrix_m_prime_data(acnt-1))
                        tot_haz_udurationmatrix_s_data(acnt)=1.0-(tot_udurationmatrix_s_data(acnt)/tot_udurationmatrix_s_data(acnt-1))    
                        tot_haz_udurationmatrix_s_young_data(acnt)=1.0-(tot_udurationmatrix_s_young_data(acnt)/tot_udurationmatrix_s_young_data(acnt-1))
                        tot_haz_udurationmatrix_s_prime_data(acnt)=1.0-(tot_udurationmatrix_s_prime_data(acnt)/tot_udurationmatrix_s_prime_data(acnt-1))
                    END DO 

!================================
! REPEAT MOBILITY
!================================

                      

                     
 
 
  
                    ! repeat_mobility.xlsx
                      !noccafternocc_ave_data=0.634 
                      !noccafterocc_ave_data=1.0_8- 0.544 
                      !noccafternocc_y_ave_data=0.617 
                      !noccafterocc_y_ave_data=1.0_8- 0.645 
                      !noccafternocc_p_ave_data=0.656 
                      !noccafterocc_p_ave_data=1.0_8- 0.491 


                    
                    !
                    !! USE CORRECTED FLOWS (Above were the corrected data stats)
                    ! repeat_mobility.xlsx
                      tot_corr_noccafternocc_data=0.634	                ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.611068578	
                    tot_corr_noccafternocc_y_data=0.617	            ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.530682485	
                    tot_corr_noccafternocc_p_data=0.656               ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.661386914
                    
                    !! MOVING TO A THIRD OCCUPATION                         !!
                    tot_corr_noccafterocc_data=0.436	               ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.399579237	
                    tot_corr_noccafterocc_y_data=0.355  	            ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.463656464		
                    tot_corr_noccafterocc_p_data=0.501               ! <- average; -> llbfr_llaft_aft_basic__noconstr: 0.373485916
                    
                      
!=============================================
!  OCC MOBILITY / U. DURATION PROFILES
!=============================================

! durationprofiles_mog.dta                    
                    
! uncorrected profile, ALL 
tot_durationmatrix_cocc_data(1)=0.53163553
tot_durationmatrix_cocc_data(2)=0.54556726
tot_durationmatrix_cocc_data(3)=0.55419567
tot_durationmatrix_cocc_data(4)=0.57559704
tot_durationmatrix_cocc_data(5)=0.58605247
tot_durationmatrix_cocc_data(6)=0.59150656
tot_durationmatrix_cocc_data(7)=0.59596633
tot_durationmatrix_cocc_data(8)=0.60511623
tot_durationmatrix_cocc_data(9)=0.61532197
tot_durationmatrix_cocc_data(10)=0.62233147
tot_durationmatrix_cocc_data(11)=0.6241119
tot_durationmatrix_cocc_data(12)=0.63939085
                 
                    
! MEASUREMENT ERROR CORRECTED MOBILITY PROFILE, ALL

tot_durationmatrix_cr_cocc_data(1)=0.44461668
tot_durationmatrix_cr_cocc_data(2)=0.46084822
tot_durationmatrix_cr_cocc_data(3)=0.47108718
tot_durationmatrix_cr_cocc_data(4)=0.49752938
tot_durationmatrix_cr_cocc_data(5)=0.51093206
tot_durationmatrix_cr_cocc_data(6)=0.51755245
tot_durationmatrix_cr_cocc_data(7)=0.5226899
tot_durationmatrix_cr_cocc_data(8)=0.53431296
tot_durationmatrix_cr_cocc_data(9)=0.54648786
tot_durationmatrix_cr_cocc_data(10)=0.55609267
tot_durationmatrix_cr_cocc_data(11)=0.55826997
tot_durationmatrix_cr_cocc_data(12)=0.57581063

! YOUNG, UNCORRECTED
tot_durationmatrix_cocc_young_data(1)=0.595755924
tot_durationmatrix_cocc_young_data(2)=0.607854525
tot_durationmatrix_cocc_young_data(3)=0.600727553
tot_durationmatrix_cocc_young_data(4)=0.613239581
tot_durationmatrix_cocc_young_data(5)=0.635127691
tot_durationmatrix_cocc_young_data(6)=0.64482873
tot_durationmatrix_cocc_young_data(7)=0.648532397
tot_durationmatrix_cocc_young_data(8)=0.668742034
tot_durationmatrix_cocc_young_data(9)=0.669031352
tot_durationmatrix_cocc_young_data(10)=0.679006171
tot_durationmatrix_cocc_young_data(11)=0.65891288
tot_durationmatrix_cocc_young_data(12)=0.724478008

! YOUNG, CORRECTED
tot_durationmatrix_cr_cocc_young_data(1)=0.52562291
tot_durationmatrix_cr_cocc_young_data(2)=0.53946231
tot_durationmatrix_cr_cocc_young_data(3)=0.52995982
tot_durationmatrix_cr_cocc_young_data(4)=0.54607462
tot_durationmatrix_cr_cocc_young_data(5)=0.57401911
tot_durationmatrix_cr_cocc_young_data(6)=0.58638562
tot_durationmatrix_cr_cocc_young_data(7)=0.59046664
tot_durationmatrix_cr_cocc_young_data(8)=0.61685697
tot_durationmatrix_cr_cocc_young_data(9)=0.61767613
tot_durationmatrix_cr_cocc_young_data(10)=0.63083192
tot_durationmatrix_cr_cocc_young_data(11)=0.60462143
tot_durationmatrix_cr_cocc_young_data(12)=0.68907302


! PRIME, UNCORRECTED 
tot_durationmatrix_cocc_prime_data(1)=0.49411049
tot_durationmatrix_cocc_prime_data(2)=0.51277441
tot_durationmatrix_cocc_prime_data(3)=0.52980961
tot_durationmatrix_cocc_prime_data(4)=0.55579007
tot_durationmatrix_cocc_prime_data(5)=0.56245731
tot_durationmatrix_cocc_prime_data(6)=0.56649933
tot_durationmatrix_cocc_prime_data(7)=0.5724063
tot_durationmatrix_cocc_prime_data(8)=0.56804478
tot_durationmatrix_cocc_prime_data(9)=0.5786245
tot_durationmatrix_cocc_prime_data(10)=0.5772841
tot_durationmatrix_cocc_prime_data(11)=0.57994647
tot_durationmatrix_cocc_prime_data(12)=0.56503683

          
! PRIME-AGED DATA, CORRECTED 
tot_durationmatrix_cr_cocc_prime_data(1)=0.39720409
tot_durationmatrix_cr_cocc_prime_data(2)=0.4191289
tot_durationmatrix_cr_cocc_prime_data(3)=0.44014994
tot_durationmatrix_cr_cocc_prime_data(4)=0.47203739
tot_durationmatrix_cr_cocc_prime_data(5)=0.48040587
tot_durationmatrix_cr_cocc_prime_data(6)=0.48549637
tot_durationmatrix_cr_cocc_prime_data(7)=0.49274613
tot_durationmatrix_cr_cocc_prime_data(8)=0.48710611
tot_durationmatrix_cr_cocc_prime_data(9)=0.49925332
tot_durationmatrix_cr_cocc_prime_data(10)=0.49861046
tot_durationmatrix_cr_cocc_prime_data(11)=0.50177958
tot_durationmatrix_cr_cocc_prime_data(12)=0.47975715
              


    cmy_cmp_data=tot_durationmatrix_cocc_young_data(1)/tot_durationmatrix_cocc_prime_data(1)
    cmy_cmp_corr_data=tot_durationmatrix_cr_cocc_young_data(1)/tot_durationmatrix_cr_cocc_prime_data(1)
    cmy_cmp_data2m=tot_durationmatrix_cocc_young_data(2)/tot_durationmatrix_cocc_prime_data(2)
    cmy_cmp_corr_data2m=tot_durationmatrix_cr_cocc_young_data(2)/tot_durationmatrix_cr_cocc_prime_data(2)
    cmy_cmp_data3m=tot_durationmatrix_cocc_young_data(3)/tot_durationmatrix_cocc_prime_data(3)
    cmy_cmp_corr_data3m=tot_durationmatrix_cr_cocc_young_data(3)/tot_durationmatrix_cr_cocc_prime_data(3)
    cmy_cmp_data4m=tot_durationmatrix_cocc_young_data(4)/tot_durationmatrix_cocc_prime_data(4)
    cmy_cmp_corr_data4m=tot_durationmatrix_cr_cocc_young_data(4)/tot_durationmatrix_cr_cocc_prime_data(4)
    cmy_cmp_data_ave=SUM(tot_durationmatrix_cocc_young_data(1:12))/SUM(tot_durationmatrix_cocc_prime_data(1:12))
    cmy_cmp_corr_data_ave=SUM(tot_durationmatrix_cr_cocc_young_data(1:12))/SUM(tot_durationmatrix_cr_cocc_prime_data(1:12))




                    noccafterall_ave_data=1.0-sum(tot_durationmatrix_cocc_data(2:4))/3.0_8
                    noccafterall_y_ave_data=1.0-sum(tot_durationmatrix_cocc_young_data(2:4))/3.0_8
                    noccafterall_p_ave_data=1.0-sum(tot_durationmatrix_cocc_prime_data(2:4))/3.0_8



                    !=======================================
                    !   INCOMPLETE DURATION DISTRIBUTION
                    !=======================================

                    !table1_table11_online_appendix_incompl_durdistr.txt7
                    ! ALL
                    uprop_3m_ave_data=0.432
                    uprop_5m_ave_data=0.667
                    uprop_9m_ave_data=0.202
                    uprop_13m_ave_data=0.081
                    uprop_g13m_ave_data=0.049

                    ! YOUNG
                    uprop_yng_3m_ave_data=0.469
                    uprop_yng_5m_ave_data=0.708
                    uprop_yng_9m_ave_data=0.191
                    uprop_yng_13m_ave_data=0.068
                    uprop_yng_g13m_ave_data=0.033

                    ! PRIME
                    uprop_prm_3m_ave_data=0.414
                    uprop_prm_5m_ave_data=0.650
                    uprop_prm_9m_ave_data=0.208
                    uprop_prm_13m_ave_data=0.087
                    uprop_prm_g13m_ave_data=0.055


                    ! BLS groups, not double checked
                    ult5wk_ave_data=0.3646
                    u5lt15wk_ave_data=0.2983
                    u15lt27wk_ave_data=0.1436
                    ugt27wk_ave_data=0.1932

                    !=======================================
                    !   INCOMPLETE DURATION DISTRIBUTION, BUSINESS CYCLE 
                    !=======================================

                    
                    !table5A_durdistr_elas_apptable11B.txt
                    u3m_unemp_elasticity_data=-0.464
                    u5m_unemp_elasticity_data=-0.363
                    u9m_unemp_elasticity_data=0.320
                    u13m_unemp_elasticity_data=0.864
                    ug13m_unemp_elasticity_data=1.375

                    !table5A_hpdurdistr_semielas_apptable11B.txt
                    hp_uprop3m_unemp_semielasticity_instdata= -0.167
                    hp_uprop5m_unemp_semielasticity_instdata= -0.184
                    hp_uprop9m_unemp_semielasticity_instdata=  0.076
                    hp_uprop13m_unemp_semielasticity_instdata= 0.072
                    hp_upropg13m_unemp_semielasticity_instdata=0.043
                    
                    sm_hp_uprop3m_unemp_semielasticity_data= -0.169
                    sm_hp_uprop5m_unemp_semielasticity_data= -0.186
                    sm_hp_uprop9m_unemp_semielasticity_data=  0.0771
                    sm_hp_uprop13m_unemp_semielasticity_data= 0.0725
                    sm_hp_upropg13m_unemp_semielasticity_data=0.0437

    !               
    !================================
    ! VACANCY, MATCHING FUNCTION
    !==============================


    elmatch_data=0.5
    

    !=====================================
    !  UNEMPLOYMENT DURATION RESPONSE
    !========================================

        ! We use appendixtable_udur2_sept2018.xlsx and appendixtable_udur_2018_cycle_FORTEXT_2209.xlsx
        ! for the numbers we use those that control for s/d occupations. MMO_W5c_STAB sheet

    ! CROSS-SECTIONAL AVERAGES IN THE DATA
                !rel_udur_moverstayer_calibration.xlsx                    
    udur_occ_ave_data=4.588
    udur_nocc_ave_data=4.029
    rel_udur_move_stay_data=udur_occ_ave_data/udur_nocc_ave_data

    !table5_panelB_udur_cycle.xls
    udur_nocc_with_lu_data=1.657           !wp_u_lurdwvcrlinst_dlt18 stayer
    udur_occ_with_lu_data=2.041           !wp_u_lurdwvcrlinst_dlt18 mover
    udur_nocc_with_hplu_data=2.544           !wp_u_hplurdwvcrlinst_dlt18 stayer
    udur_occ_with_hplu_data=3.152           ! wp_u_hplurdwvcrlinst_dlt18 mover


     
!=============================================
!  OCC MOBILITY / U. DURATION PROFILES, BUSINESS CYCLE 
!=============================================

! cycldurationshift_mog.dta
tot_durationmatrix_cr_cocc_p33_data(1)=0.45931155
tot_durationmatrix_cr_cocc_p33_data(2)=0.48496445
tot_durationmatrix_cr_cocc_p33_data(3)=0.50766897
tot_durationmatrix_cr_cocc_p33_data(4)=0.52600756
tot_durationmatrix_cr_cocc_p33_data(5)=0.54206133
tot_durationmatrix_cr_cocc_p33_data(6)=0.5567437
tot_durationmatrix_cr_cocc_p33_data(7)=0.56916078
tot_durationmatrix_cr_cocc_p33_data(8)=0.58015657
tot_durationmatrix_cr_cocc_p33_data(9)=0.59010559
tot_durationmatrix_cr_cocc_p33_data(10)=0.59702054
tot_durationmatrix_cr_cocc_p33_data(11)=0.59525462
tot_durationmatrix_cr_cocc_p33_data(12)=0.58289572
    

tot_durationmatrix_cr_cocc_p67_data(1)=0.43294773
tot_durationmatrix_cr_cocc_p67_data(2)=0.44560695
tot_durationmatrix_cr_cocc_p67_data(3)=0.45817678
tot_durationmatrix_cr_cocc_p67_data(4)=0.47072161
tot_durationmatrix_cr_cocc_p67_data(5)=0.48330699
tot_durationmatrix_cr_cocc_p67_data(6)=0.49604513
tot_durationmatrix_cr_cocc_p67_data(7)=0.50898193
tot_durationmatrix_cr_cocc_p67_data(8)=0.52130709
tot_durationmatrix_cr_cocc_p67_data(9)=0.53117104
tot_durationmatrix_cr_cocc_p67_data(10)=0.5365753
tot_durationmatrix_cr_cocc_p67_data(11)=0.53504677
tot_durationmatrix_cr_cocc_p67_data(12)=0.52710055



    
    
    
    END SUBROUTINE set_auxdata_moments

    
    
    
    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!         N U N                               !!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
    
    
SUBROUTINE set_auxdata_moments_nun
            !! Overwrite only those moments for which we are using NuN-specific targets

                        ! table7_online_appx_mean_nunemp.txt
                      u_ave_data=0.051
                      u_data=0.051              ! unemployment rate

                      



! from survivalprofiles_and_hazards.dta

!ALL
tot_durationmatrix_data(1)=1.0
tot_durationmatrix_data(2)=0.8362813
tot_durationmatrix_data(3)=0.69132379
tot_durationmatrix_data(4)=0.57059684
tot_durationmatrix_data(5)=0.48867258
tot_durationmatrix_data(6)=0.4251416
tot_durationmatrix_data(7)=0.37608772
tot_durationmatrix_data(8)=0.32631545
tot_durationmatrix_data(9)=0.29597961
tot_durationmatrix_data(10)=0.26548717
tot_durationmatrix_data(11)=0.23740735
tot_durationmatrix_data(12)=0.2132707
tot_durationmatrix_data(13)=0.19602495
tot_durationmatrix_data(14)=0.17969192
tot_durationmatrix_data(15)=0.16334668
tot_durationmatrix_data(16)=0.15326209
tot_durationmatrix_data(17)=0.14288807
tot_durationmatrix_data(18)=0.13394352
tot_durationmatrix_data(19)=0.12677797
tot_durationmatrix_data(20)=0.11718603


!! YOUNG
tot_durationmatrix_young_data(1)=1.0
tot_durationmatrix_young_data(2)=0.79684325
tot_durationmatrix_young_data(3)=0.63398036
tot_durationmatrix_young_data(4)=0.51481518
tot_durationmatrix_young_data(5)=0.44775001
tot_durationmatrix_young_data(6)=0.38075827
tot_durationmatrix_young_data(7)=0.34308826
tot_durationmatrix_young_data(8)=0.29008063
tot_durationmatrix_young_data(9)=0.26126183
tot_durationmatrix_young_data(10)=0.2348609
tot_durationmatrix_young_data(11)=0.20164806
tot_durationmatrix_young_data(12)=0.17208745
tot_durationmatrix_young_data(13)=0.14899292
tot_durationmatrix_young_data(14)=0.14021483
tot_durationmatrix_young_data(15)=0.12285287
tot_durationmatrix_young_data(16)=0.11612603
tot_durationmatrix_young_data(17)=0.10253773
tot_durationmatrix_young_data(18)=0.09421879
tot_durationmatrix_young_data(19)=0.08634424
tot_durationmatrix_young_data(20)=0.08039214
!! PRIME
tot_durationmatrix_prime_data(1)=1.0
tot_durationmatrix_prime_data(2)=0.85348191
tot_durationmatrix_prime_data(3)=0.70596688
tot_durationmatrix_prime_data(4)=0.58677909
tot_durationmatrix_prime_data(5)=0.48932136
tot_durationmatrix_prime_data(6)=0.43608757
tot_durationmatrix_prime_data(7)=0.38070752
tot_durationmatrix_prime_data(8)=0.33483055
tot_durationmatrix_prime_data(9)=0.30447322
tot_durationmatrix_prime_data(10)=0.2680955
tot_durationmatrix_prime_data(11)=0.23974304
tot_durationmatrix_prime_data(12)=0.21614234
tot_durationmatrix_prime_data(13)=0.20323091
tot_durationmatrix_prime_data(14)=0.18104857
tot_durationmatrix_prime_data(15)=0.1677227
tot_durationmatrix_prime_data(16)=0.15695138
tot_durationmatrix_prime_data(17)=0.14746028
tot_durationmatrix_prime_data(18)=0.13486105
tot_durationmatrix_prime_data(19)=0.12578878
tot_durationmatrix_prime_data(20)=0.11480958


    !=============================================
    ! COMPOSITION WITH UNEMPLOYMENT DURATION
    !=============================================

! from cycldurationshift_mog_nun_males.dta
! ALL MALES, NUN 
tot_durationmatrix_cocc_data(1)=0.53739595
tot_durationmatrix_cocc_data(2)=0.55052469
tot_durationmatrix_cocc_data(3)=0.56556379
tot_durationmatrix_cocc_data(4)=0.59027757
tot_durationmatrix_cocc_data(5)=0.6013212
tot_durationmatrix_cocc_data(6)=0.61003068
tot_durationmatrix_cocc_data(7)=0.61354106
tot_durationmatrix_cocc_data(8)=0.62267
tot_durationmatrix_cocc_data(9)=0.63712534
tot_durationmatrix_cocc_data(10)=0.65007882
tot_durationmatrix_cocc_data(11)=0.65200001
tot_durationmatrix_cocc_data(12)=0.67661423


!! MEASUREMENT ERROR CORRECTED MOBILITY PROFILE
tot_durationmatrix_cr_cocc_data(1)=0.44605148
tot_durationmatrix_cr_cocc_data(2)=0.46204253
tot_durationmatrix_cr_cocc_data(3)=0.48089977
tot_durationmatrix_cr_cocc_data(4)=0.51203824
tot_durationmatrix_cr_cocc_data(5)=0.5260023
tot_durationmatrix_cr_cocc_data(6)=0.53666867
tot_durationmatrix_cr_cocc_data(7)=0.54075963
tot_durationmatrix_cr_cocc_data(8)=0.5522759
tot_durationmatrix_cr_cocc_data(9)=0.57084365
tot_durationmatrix_cr_cocc_data(10)=0.58728183
tot_durationmatrix_cr_cocc_data(11)=0.59067874
tot_durationmatrix_cr_cocc_data(12)=0.61977481




! YOUNG MALES
tot_durationmatrix_cr_cocc_young_data(1)=0.51330857
tot_durationmatrix_cr_cocc_young_data(2)=0.51756683
tot_durationmatrix_cr_cocc_young_data(3)=0.5244965
tot_durationmatrix_cr_cocc_young_data(4)=0.54728086
tot_durationmatrix_cr_cocc_young_data(5)=0.57675917
tot_durationmatrix_cr_cocc_young_data(6)=0.58367477
tot_durationmatrix_cr_cocc_young_data(7)=0.59350988
tot_durationmatrix_cr_cocc_young_data(8)=0.60137958
tot_durationmatrix_cr_cocc_young_data(9)=0.61400928
tot_durationmatrix_cr_cocc_young_data(10)=0.62685052
tot_durationmatrix_cr_cocc_young_data(11)=0.62095438
tot_durationmatrix_cr_cocc_young_data(12)=0.67922902


!! PRIME-AGED DATA
tot_durationmatrix_cr_cocc_prime_data(1)=0.39739906
tot_durationmatrix_cr_cocc_prime_data(2)=0.42291304
tot_durationmatrix_cr_cocc_prime_data(3)=0.44725023
tot_durationmatrix_cr_cocc_prime_data(4)=0.48478776
tot_durationmatrix_cr_cocc_prime_data(5)=0.49184681
tot_durationmatrix_cr_cocc_prime_data(6)=0.50319074
tot_durationmatrix_cr_cocc_prime_data(7)=0.49968134
tot_durationmatrix_cr_cocc_prime_data(8)=0.50895049
tot_durationmatrix_cr_cocc_prime_data(9)=0.51854819
tot_durationmatrix_cr_cocc_prime_data(10)=0.53884912
tot_durationmatrix_cr_cocc_prime_data(11)=0.54953941
tot_durationmatrix_cr_cocc_prime_data(12)=0.54474039



    !cmy_cmp_data=tot_durationmatrix_cocc_young_data(1)/tot_durationmatrix_cocc_prime_data(1)
    cmy_cmp_corr_data=tot_durationmatrix_cr_cocc_young_data(1)/tot_durationmatrix_cr_cocc_prime_data(1)
    !cmy_cmp_data2m=tot_durationmatrix_cocc_young_data(2)/tot_durationmatrix_cocc_prime_data(2)
    cmy_cmp_corr_data2m=tot_durationmatrix_cr_cocc_young_data(2)/tot_durationmatrix_cr_cocc_prime_data(2)
    !cmy_cmp_data3m=tot_durationmatrix_cocc_young_data(3)/tot_durationmatrix_cocc_prime_data(3)
    cmy_cmp_corr_data3m=tot_durationmatrix_cr_cocc_young_data(3)/tot_durationmatrix_cr_cocc_prime_data(3)
    !cmy_cmp_data4m=tot_durationmatrix_cocc_young_data(4)/tot_durationmatrix_cocc_prime_data(4)
    cmy_cmp_corr_data4m=tot_durationmatrix_cr_cocc_young_data(4)/tot_durationmatrix_cr_cocc_prime_data(4)
    !cmy_cmp_data_ave=SUM(tot_durationmatrix_cocc_young_data(1:12))/SUM(tot_durationmatrix_cocc_prime_data(1:12))
    cmy_cmp_corr_data_ave=SUM(tot_durationmatrix_cr_cocc_young_data(1:12))/SUM(tot_durationmatrix_cr_cocc_prime_data(1:12))



!! ===========  CYCLICAL PART


!cycldurationshift_mog_nun_males.dta
! GOOD TIMES, CORRECTED
tot_durationmatrix_cr_cocc_p33_data(1)=0.45415569
tot_durationmatrix_cr_cocc_p33_data(2)=0.47402325
tot_durationmatrix_cr_cocc_p33_data(3)=0.49288156
tot_durationmatrix_cr_cocc_p33_data(4)=0.52231393
tot_durationmatrix_cr_cocc_p33_data(5)=0.54564983
tot_durationmatrix_cr_cocc_p33_data(6)=0.5574596
tot_durationmatrix_cr_cocc_p33_data(7)=0.54596309
tot_durationmatrix_cr_cocc_p33_data(8)=0.54406508
tot_durationmatrix_cr_cocc_p33_data(9)=0.54231845
tot_durationmatrix_cr_cocc_p33_data(10)=0.53770036
tot_durationmatrix_cr_cocc_p33_data(11)=0.56580398
tot_durationmatrix_cr_cocc_p33_data(12)=0.6189906


!BAD TIMES, CORRECTED 
tot_durationmatrix_cr_cocc_p67_data(1)=0.44106118
tot_durationmatrix_cr_cocc_p67_data(2)=0.458163
tot_durationmatrix_cr_cocc_p67_data(3)=0.47711718
tot_durationmatrix_cr_cocc_p67_data(4)=0.50808432
tot_durationmatrix_cr_cocc_p67_data(5)=0.51242163
tot_durationmatrix_cr_cocc_p67_data(6)=0.53076539
tot_durationmatrix_cr_cocc_p67_data(7)=0.53600518
tot_durationmatrix_cr_cocc_p67_data(8)=0.55525052
tot_durationmatrix_cr_cocc_p67_data(9)=0.57781208
tot_durationmatrix_cr_cocc_p67_data(10)=0.60801246
tot_durationmatrix_cr_cocc_p67_data(11)=0.60508185
tot_durationmatrix_cr_cocc_p67_data(12)=0.63869681

    ! CORRECTED STATS FOR MOBILITY
    occmob_3m_p33_data=tot_durationmatrix_cr_cocc_p33_data(3)
    occmob_3m_p67_data=tot_durationmatrix_cr_cocc_p67_data(3)
    occmob_6m_p33_data=tot_durationmatrix_cr_cocc_p33_data(6)
    occmob_6m12_p67_data=(tot_durationmatrix_cr_cocc_p67_data(6)+tot_durationmatrix_cr_cocc_p67_data(7)+tot_durationmatrix_cr_cocc_p67_data(8)+ &
                            tot_durationmatrix_cr_cocc_p67_data(9)+tot_durationmatrix_cr_cocc_p67_data(10)+tot_durationmatrix_cr_cocc_p67_data(11)+&
                            tot_durationmatrix_cr_cocc_p67_data(12))/7.0_8



END SUBROUTINE set_auxdata_moments_nun

END MODULE mod_auxdata_moments
