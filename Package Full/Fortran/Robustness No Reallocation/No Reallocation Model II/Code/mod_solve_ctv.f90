
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright 2023 Ludo Visschers and Carlos Carrillo-Tudela
!
!
! Redistribution and use in source and binary forms, with or without 
!	modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, 
!	this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice, 
!	this list of conditions and the following disclaimer in the documentation 
!	and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors 
!	may be used to endorse or promote products derived from this software 
!	without specific prior written permission.
!
! 4. If using this code or its results (whole or in part, with or without 
! 	modifications) in academic work, please cite:  
!		Carrillo-Tudela, Carlos and Ludo Visschers, "Unemployment and Endogenous 
!		Reallocation over the Business Cycle" 
!	in its published version, in the references of the publications associated 
!	with aforementioned academic work.
!
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    
!********************************************************************************************************
!                                                                           T I M I N G
!
!
!           ----------|I|-------------------|------------------------------|-----------------------------|------------------------|------------------------------|II|------
!                           beginning of period                REALLOCATION                                        PRODUCTION, PAYMENTS
!                                            BREAKUP DECISIONS                          SEARCH AND MATCHING                                    beginning of next period
!                                             ~~newly unemployed CANNOT search
!                                           experience increase is realized
!********************************************************************************************************


!include 'mkl_vsl.f90'
include "errcheck.inc"


MODULE mod_solve_ctv



!USE luxury
USE MKL_VSL_TYPE
USE MKL_VSL

USE modglobal_ctv_eff
USE ctv_grid_mod
USE ctv_grid_mod_VE_VU
USE mod_tauchen_ctv
USE mod_auxdata_moments
USE mod_backward_ctv
!USE omp_lib
!USE ifport
!USE cmaes_param_mod
!USE discretizeAR1           ! ROUWENHORST DISCRETIZATION

!#ifdef __HAVE_MPI__
!      USE MPI
!#endif	


IMPLICIT NONE


!#ifdef __HAVE_MPI__
!      INCLUDE "mpif.h"
!#endif	
!  FILES PRODUCED BY THIS PROGRAM
!       UNIT=11, data.txt
!       UNIT=19, CHANGING FILES: wage.txt, ev_ve.txt
!       UNIT=20, tauchen.txt
!       OPEN(UNIT=21, file="quarterly_timeseries.csv", status="replace", form="formatted")
!       OPEN(UNIT=22, file="results2.txt", status="old", form="formatted", position="append")
!       UNIT=29, reservation.txt
!       UNIT=30, duration.txt
!       UNIT=39, textout.txt ---> opened in main_ctv!!!!
!       OPEN(UNIT=49, file='unemp_dist.txt', form='formatted', status='replace')
!       OPEN(UNIT=50, file='unemp_transmatrix.txt', form='formatted', status='replace')
!       OPEN(UNIT=51, file='occmob.txt', form='formatted', status='replace')
!       OPEN(UNIT=52, file='ave_u_dur.txt', form='formatted', status='replace')
!       OPEN(UNIT=53, file='u_dur_weekly.txt', form='formatted', status='replace')
!       OPEN(UNIT=54, file='random_test.txt', form='formatted', status='replace')
!       OPEN(UNIT=55, file='island_random.txt', form='formatted', status='replace')
!       OPEN(UNIT=56, file=*paper outputs*, form='formatted', status='replace')
!       OPEN(UNIT=59, file='dist_pars.txt', form='formatted', status='replace')
!       OPEN(UNIT=60, file='cutoffs.csv', form='formatted', status='replace')
!       OPEN(UNIT=61, file='upz_dist.csv', form='formatted', status='replace')
!       OPEN(UNIT=62, file='epz_dist.csv', form='formatted', status='replace')
!       OPEN(UNIT=63, file='upz_y_dist.csv', form='formatted', status='replace')
!       OPEN(UNIT=64, file='epz_y_dist.csv', form='formatted', status='replace')
!       OPEN(UNIT=65, file='upz_p_dist.csv', form='formatted', status='replace')
!       OPEN(UNIT=66, file='epz_p_dist.csv', form='formatted', status='replace')
!       OPEN(unit=81, file='monthly_series_nonnormalized.txt', status='replace', form='formatted')
!       OPEN(UNIT=88, file='yearly_data', form='formatted', status='replace')
!       OPEN(UNIT=93, file='monthlyseries2.txt', form='formatted', status='replace')
!       OPEN(unit=94, file='monthly_series_nonnormalized.txt', status='replace', form='formatted')
!       OPEN(unit=97, file='workerhistory.txt', status='replace', form='formatted')
!       OPEN(UNIT=99, file="parresult3.txt", status="new", form="formatted")
!       OPEN(UNIT=59, file="error.txt", status="new", form="formatted")  in MAIN_CTV

CONTAINS

    SUBROUTINE CTV_function_simple(thetal,mom,mom1_dist,mpthread)  !,mom,mom1_dist, mom2_dist, mom3_dist, stat_switch) THE REST OF THE VARIABLES WILL COME
                                        ! theta is the set of parameters fed into this system
                                        ! mom is the actual moments (in the real data)
                                        ! momdists are the moment distances




    !Arguments
    REAL(8), DIMENSION(:), INTENT(in) :: thetal          !  Vector of moments put into the model
    REAL(8), DIMENSION(:), INTENT(out) :: mom           !
    REAL(8), INTENT(out) :: mom1_dist !,  mom2_dist, mom3_dist                    !
    INTEGER, INTENT(in) :: mpthread

    INTEGER, PARAMETER :: nmom_local=74
    REAL(8), DIMENSION(nmom_local) :: mom_local
    REAL(8), DIMENSION(nmom_local) :: mom_data
    REAL(8), DIMENSION(nmom_local, nmom_local) :: omega
    REAL(8), DIMENSION(nmom_local) :: momweightl
    !REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VUl        ! (ppts, xpts, zpts, tmax)        Value of unemployment
    !REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VEl        ! (ppts, xpts, zpts, tmax)           Joint value of employment, conditional on p, z, x
    !REAL(8), DIMENSION(ppts,xpts,zpts) :: VUl
    !REAL(8), DIMENSION(ppts,xpts,zpts) :: VEl
    !INTEGER(4), INTENT(in):: stat_switch

    !vars, parameters, but main parameters in global_dsage
    !counters
    INTEGER :: icnt                      ! island counter
    INTEGER(4):: tcnt, tcnt2                   !Counter of period
    INTEGER(4):: zcnt                   !idiosyncratic shock count
    INTEGER(4):: pcnt                  ! aggregate productivity count
    INTEGER(4):: xcnt, x2cnt                  ! experience count
    INTEGER  :: p2cnt
    INTEGER :: z2cnt

    REAL(8) :: zcorrection

    ! second run, increased z-precision

    REAL(8) :: zprecision_lb, zprecision_ub, tempreal_dd, sep_cutoff, skilldep_cutoff
    REAL(8), DIMENSION(xpts) :: sep_cutoff_vec


    ! convergence
    REAL(8) :: convdistance

    !For simulations STILL TO CHANGE
    REAL, DIMENSION(MAX(tmax_sim, tmax_sim2)) :: prob



    ! RANDOM NUMBER GENERATING
    INTEGER :: tmphour, tmpminute, tmpsecond, tmphund
    INTEGER :: randomseedy
    INTEGER, DIMENSION(25) :: isvec


    INTEGER(4) :: i, isl, t, tempint, tempint3, tempint0, tempint2, tempint1
    INTEGER :: tmax_sim2_quarterly, tmax_qtr

    INTEGER(2), ALLOCATABLE, DIMENSION(:) :: aggprod_ind




    ! SIMULATIONS
   REAL, DIMENSION(tmax_sim2) :: jprobs, sepprobs, zprobs
   INTEGER, DIMENSION(tmax_sim2) :: e_sim, island_sim, hcten_sim, hcind_sim, duration_sim, e_duration_sim, occmob_ind, corr_occmob_ind
   REAL(8), DIMENSION(tmax_sim2) ::  wage_sim

   REAL(8) :: monthind
    ! unemployment duration: tot_durationmatrix --> unemployment survival rates
    ! composition of the unemployed: tot_durationmatrix_cocc/nocc --> cm/cs - uduration
    ! unemployment duration of occ stayers and changers: tot_uduration_s/m

   !===
   ! RAW OCCUPATIONAL MOBILITY MEASURES, incl. MEASUREMENT ERROR
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix, tot_durationmatrix_cocc  , tot_durationmatrix_nocc, tot_durationmatrix2
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cocc_p33, tot_durationmatrix_cocc_p50  , tot_durationmatrix_cocc_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_nocc_p33, tot_durationmatrix_nocc_p50  , tot_durationmatrix_nocc_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cocc_young_p33, tot_durationmatrix_cocc_young_p50  , tot_durationmatrix_cocc_young_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_nocc_young_p33, tot_durationmatrix_nocc_young_p50  , tot_durationmatrix_nocc_young_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cocc_prime_p33, tot_durationmatrix_cocc_prime_p50  , tot_durationmatrix_cocc_prime_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_nocc_prime_p33, tot_durationmatrix_nocc_prime_p50  , tot_durationmatrix_nocc_prime_p67

   !===
   ! CORRECTED OCCUPATIONAL MOBILITY MEASURES, incl. MEASUREMENT ERROR
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr, tot_durationmatrix_cr_cocc  , tot_durationmatrix_cr_nocc
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_cocc_p33, tot_durationmatrix_cr_cocc_p50  , tot_durationmatrix_cr_cocc_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_nocc_p33, tot_durationmatrix_cr_nocc_p50  , tot_durationmatrix_cr_nocc_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_cocc_young_p33, tot_durationmatrix_cr_cocc_young_p50  , tot_durationmatrix_cr_cocc_young_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_nocc_young_p33, tot_durationmatrix_cr_nocc_young_p50  , tot_durationmatrix_cr_nocc_young_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_cocc_prime_p33, tot_durationmatrix_cr_cocc_prime_p50  , tot_durationmatrix_cr_cocc_prime_p67
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_nocc_prime_p33, tot_durationmatrix_cr_nocc_prime_p50  , tot_durationmatrix_cr_nocc_prime_p67


   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_young, tot_durationmatrix_cocc_young , tot_durationmatrix_nocc_young
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_prime, tot_durationmatrix_cocc_prime , tot_durationmatrix_nocc_prime
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_young, tot_durationmatrix_cr_cocc_young , tot_durationmatrix_cr_nocc_young
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix_cr_prime, tot_durationmatrix_cr_cocc_prime , tot_durationmatrix_cr_nocc_prime

   REAL(8), DIMENSION(0:24) :: tot_udurationmatrix_m_young, tot_udurationmatrix_s_young , tot_udurationmatrix_m
   REAL(8), DIMENSION(0:24) :: tot_udurationmatrix_m_prime, tot_udurationmatrix_s_prime , tot_udurationmatrix_s

   REAL(8), DIMENSION(0:24) :: tot_durationmatrix2_cocc, tot_durationmatrix2_cocc_young , tot_durationmatrix2_nocc_young
   REAL(8), DIMENSION(0:24) :: tot_durationmatrix2_nocc, tot_durationmatrix2_cocc_prime , tot_durationmatrix2_nocc_prime


   REAL(8), DIMENSION(0:24) :: tot_haz_udurationmatrix_m_young, tot_haz_udurationmatrix_s_young , tot_haz_udurationmatrix_m
   REAL(8), DIMENSION(0:24) :: tot_haz_udurationmatrix_m_prime, tot_haz_udurationmatrix_s_prime , tot_haz_udurationmatrix_s
   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix, tot_haz_durationmatrix_cocc  , tot_haz_durationmatrix_nocc
   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix_young, tot_haz_durationmatrix_cocc_young , tot_haz_durationmatrix_nocc_young
   REAL(8), DIMENSION(0:24) :: tot_haz_durationmatrix_prime, tot_haz_durationmatrix_cocc_prime , tot_haz_durationmatrix_nocc_prime
    REAL(8), DIMENSION(0:24, 2) :: tot_haz_durationmatrix2


   REAL(8) :: subseqreall, returnstenure5y, returnstenure10y, ult5wk, u5lt15wk, u15lt27wk, ugt27wk, tot_ult5wk, tot_u5lt15wk, tot_u15lt27wk, tot_ugt27wk

   ! LOG WAGE DISPERSION COMPARISON
   REAL(8) :: logwage_disp_all, logwage_ave_all, logwage_disp_highoccten, logwage_ave_highoccten, logwage_disp_uhires, logwage_ave_uhires
   REAL(8) :: logwage_disp_diff_all, logwage_disp_diff_highoccten
   INTEGER :: logwage_all_counter, logwage_highoccten_counter, logwage_uhires_counter



   REAL(8), DIMENSION(0:7) :: empduration,  empduration_y, empduration_p
   REAL(8), DIMENSION(0:7) :: empduration_shuspell, empduration_luspell



   REAL(8), DIMENSION(0:7) :: empduration_occ, empduration_nocc, empduration_occ_y, empduration_occ_p, empduration_nocc_y, empduration_nocc_p
   REAL(8), DIMENSION(0:7) :: empduration_occ_shuspell, empduration_nocc_shuspell, empduration_occ_luspell, empduration_nocc_luspell


   REAL(8) :: reemployhaz_sas, reemployhaz_sam, reemployhaz_mas, reemployhaz_mam
   REAL(8) :: reemployhaz_sas_y, reemployhaz_sam_y, reemployhaz_mas_y, reemployhaz_mam_y
   REAL(8) :: reemployhaz_sas_p, reemployhaz_sam_p, reemployhaz_mas_p, reemployhaz_mam_p

   REAL(8), DIMENSION(0:7) :: duration2
   REAL(8), DIMENSION(0:7) :: duration2_young, duration2_prime, duration2_move, duration2_stay, duration2_move_young, duration2_stay_young, duration2_move_prime, duration2_stay_prime

   REAL(8) :: ult5wk_2, u5lt15wk_2, u15lt27wk_2, ugt27wk_2
   INTEGER :: subseqreallcounter, returnstenure5ycounter, returnstenure10ycounter
   INTEGER :: counter_u_window, counter_u_window_all, counter_uocc_window, counter_uocc_window_all, counter_unocc_window, counter_unocc_window_all
   INTEGER :: counter_u_window_all_y, counter_u_window_all_p, counter_u_window_y, counter_u_window_p
   REAL(8) :: uproportion_window, uoccproportion_window, unoccproportion_window, uproportion_window_y, uproportion_window_p


    REAL(8) :: repeat_udur_corr, repeat_udur_el, repeat_udur_corr_m, repeat_udur_el_m, repeat_udur_corr_s, repeat_udur_el_s
    REAL(8) :: repeat_udur_corr_sam, repeat_udur_el_sam, repeat_udur_corr_sas, repeat_udur_el_sas
    REAL(8) :: repeat_udur_corr_mam, repeat_udur_el_mam, repeat_udur_corr_mas, repeat_udur_el_mas
    REAL(8) ::repeat_udur_corr_y, repeat_udur_el_y, repeat_udur_corr_m_y, repeat_udur_el_m_y, repeat_udur_corr_s_y, repeat_udur_el_s_y
    REAL(8) :: repeat_udur_corr_sam_y, repeat_udur_el_sam_y, repeat_udur_corr_sas_y, repeat_udur_el_sas_y
    REAL(8) :: repeat_udur_corr_mam_y, repeat_udur_el_mam_y, repeat_udur_corr_mas_y, repeat_udur_el_mas_y
    REAL(8) ::repeat_udur_corr_p, repeat_udur_el_p, repeat_udur_corr_m_p, repeat_udur_el_m_p, repeat_udur_corr_s_p, repeat_udur_el_s_p
    REAL(8) :: repeat_udur_corr_sam_p, repeat_udur_el_sam_p, repeat_udur_corr_sas_p, repeat_udur_el_sas_p
    REAL(8) :: repeat_udur_corr_mam_p, repeat_udur_el_mam_p, repeat_udur_corr_mas_p, repeat_udur_el_mas_p



    ! for correlation and elasticity calculation
    REAL(8) :: u_spell_now, u_spell_before, u_spell_before_sq, u_spell_before_am_sq, u_spell_before_as_sq, u_spell_before_mas_sq, u_spell_before_mam_sq, u_spell_before_sam_sq, u_spell_before_sas_sq
    REAL(8) :: u_spell_now_sq, u_spell_now_am_sq, u_spell_now_as_sq, u_spell_now_mas_sq, u_spell_now_mam_sq, u_spell_now_sam_sq, u_spell_now_sas_sq
    REAL(8) :: u_spell_crossterm, u_spell_crossterm_am, u_spell_crossterm_as, u_spell_crossterm_mas, u_spell_crossterm_mam, u_spell_crossterm_sam, u_spell_crossterm_sas
        !young
    REAL(8) :: u_spell_young_before_sq, u_spell_young_before_am_sq, u_spell_young_before_as_sq, u_spell_young_before_mas_sq, u_spell_young_before_mam_sq, u_spell_young_before_sam_sq, u_spell_young_before_sas_sq
    REAL(8) :: u_spell_young_now_sq, u_spell_young_now_am_sq, u_spell_young_now_as_sq, u_spell_young_now_mas_sq, u_spell_young_now_mam_sq, u_spell_young_now_sam_sq, u_spell_young_now_sas_sq
    REAL(8) :: u_spell_young_crossterm, u_spell_young_crossterm_am, u_spell_young_crossterm_as, u_spell_young_crossterm_mas, u_spell_young_crossterm_mam, u_spell_young_crossterm_sam, u_spell_young_crossterm_sas
        !old
    REAL(8) :: u_spell_prime_before_sq, u_spell_prime_before_am_sq, u_spell_prime_before_as_sq, u_spell_prime_before_mas_sq, u_spell_prime_before_mam_sq, u_spell_prime_before_sam_sq, u_spell_prime_before_sas_sq
    REAL(8) :: u_spell_prime_now_sq, u_spell_prime_now_am_sq, u_spell_prime_now_as_sq, u_spell_prime_now_mas_sq, u_spell_prime_now_mam_sq, u_spell_prime_now_sam_sq, u_spell_prime_now_sas_sq
    REAL(8) :: u_spell_prime_crossterm, u_spell_prime_crossterm_am, u_spell_prime_crossterm_as, u_spell_prime_crossterm_mas, u_spell_prime_crossterm_mam, u_spell_prime_crossterm_sam, u_spell_prime_crossterm_sas

    ! averages for centered elasticities
    REAL(8) ::u_spell_before_ave, u_spell_before_am_ave, u_spell_before_as_ave, u_spell_before_mas_ave, u_spell_before_mam_ave, u_spell_before_sam_ave, u_spell_before_sas_ave
    REAL(8) :: u_spell_now_ave, u_spell_now_am_ave, u_spell_now_as_ave, u_spell_now_mas_ave, u_spell_now_mam_ave, u_spell_now_sam_ave, u_spell_now_sas_ave
        !young
    REAL(8) :: u_spell_young_before_ave, u_spell_young_before_am_ave, u_spell_young_before_as_ave, u_spell_young_before_mas_ave, u_spell_young_before_mam_ave, u_spell_young_before_sam_ave, u_spell_young_before_sas_ave
    REAL(8) :: u_spell_young_now_ave, u_spell_young_now_am_ave, u_spell_young_now_as_ave, u_spell_young_now_mas_ave, u_spell_young_now_mam_ave, u_spell_young_now_sam_ave, u_spell_young_now_sas_ave
        !old
    REAL(8) :: u_spell_prime_before_ave, u_spell_prime_before_am_ave, u_spell_prime_before_as_ave, u_spell_prime_before_mas_ave, u_spell_prime_before_mam_ave, u_spell_prime_before_sam_ave, u_spell_prime_before_sas_ave
    REAL(8) :: u_spell_prime_now_ave, u_spell_prime_now_am_ave, u_spell_prime_now_as_ave, u_spell_prime_now_mas_ave, u_spell_prime_now_mam_ave, u_spell_prime_now_sam_ave, u_spell_prime_now_sas_ave


   REAL(8), DIMENSION(10) :: occ_ten_dist, occ_surv_rates

   REAL(8) :: profileshift_shortdur, profileshift_longdur

   ! DATA MOMENTS --- MONTHLY

   ! MONTHLY: tmax_sim2/12
   INTEGER, ALLOCATABLE, DIMENSION(:) :: unempdur_estatus
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_prime_qtr


   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u_qtr, data_e_qtr, data_uadj18m_qtr, data_ulev_qtr, data_uraw_qtr, data_lu5_qtr, data_uall_qtr, data_u_yr,data_luall5_qtr 
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ucompldur_occ_qtr, data_ucompldur_young_occ_qtr, data_ucompldur_prime_occ_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ucompldur_nocc_qtr, data_ucompldur_young_nocc_qtr, data_ucompldur_prime_nocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u_qtr_p33, data_u_qtr_p50, data_u_qtr_p67
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cocc_qtr, data_totduration_nocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cocc_young_qtr, data_totduration_nocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cocc_prime_qtr, data_totduration_nocc_prime_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_compduration_cocc_qtr, data_compduration_nocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_compduration_cocc_young_qtr, data_compduration_nocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_compduration_cocc_prime_qtr, data_compduration_nocc_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cr_cocc_qtr, data_totduration_cr_nocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cr_cocc_young_qtr, data_totduration_cr_nocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:,:) :: data_totduration_cr_cocc_prime_qtr, data_totduration_cr_nocc_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u_young_qtr, data_e_young_qtr, data_uadj18m_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u_prime_qtr, data_e_prime_qtr, data_uadj18m_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_v_qtr, data_lv5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_theta_qtr, data_ltheta5_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_jf_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_jf_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_jf_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf5_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf5_prime_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_lpocc5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_lpnocc5_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ljf_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_prod_qtr, data_wage_qtr, data_lprod5_qtr, data_lwage5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sep_qtr, data_sep_y_qtr, data_sep_p_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sep_yr, data_sep_y_yr, data_sep_p_yr, data_jf_y_yr, data_jf_p_yr, data_u_y_yr, data_u_p_yr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_lsep_qtr, data_lsep_y_qtr, data_lsep_p_qtr, data_lsep5_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_inflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_young_inflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_prime_inflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_inflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_young_inflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_prime_inflow_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_prime_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_prime_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_outflow_qtr, data_lcocc_outflow5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_young_outflow_qtr, data_lcocc_young_outflow5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_cocc_prime_outflow_qtr, data_lcocc_prime_outflow5_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_outflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_young_outflow_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_nocc_prime_outflow_qtr


   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pnocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_lpocc_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_lpnocc_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pnocc_young_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pocc_prime_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_pnocc_prime_qtr


   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_temp_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_temp_qtr2
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_temp3_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult5wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u5lt15wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_u15lt27wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ugt27wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult5wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_u5lt15wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_u15lt27wk_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ugt27wk_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult3m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult5m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult9m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult13m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ugt13m_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult3m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult5m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult9m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult13m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ugt13m_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sm5_ult3m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sm5_ult5m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sm5_ult9m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sm5_ult13m_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_sm5_ugt13m_qtr


   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult3m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult5m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult9m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult13m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ugt13m_yng_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult3m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult5m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult9m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult13m_yng_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ugt13m_yng_qtr

   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult3m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult5m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult9m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ult13m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_ugt13m_prm_qtr



   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult3m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult5m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult9m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ult13m_prm_qtr
   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_l_ugt13m_prm_qtr




   REAL(8), ALLOCATABLE, DIMENSION(:) :: data_scratch_qtr
   
   !!--------------------------------------------------------
   !! simple memory savings by adding up all monthly data
   !!--------------------------------------------------------

   REAL(8) :: tot_young_qtru, tot_prime_qtr
                ! tot_young_qtru to keep track of unemployed
   REAL(8) :: tot_u_young_qtr, tot_u_prime_qtr
   REAL(8) :: tot_costofhires, tot_hires

   REAL(8)  :: tot_occafterocc, tot_occafternocc, tot_occafterocc_y, tot_occafternocc_y, tot_occafterocc_p, tot_occafternocc_p
   REAL(8)  :: tot_noccafterocc, tot_noccafternocc, tot_noccafterocc_y, tot_noccafternocc_y, tot_noccafterocc_p, tot_noccafternocc_p
   REAL(8)  :: tot_corr_occafterocc, tot_corr_occafternocc, tot_corr_occafterocc_y, tot_corr_occafternocc_y, tot_corr_occafterocc_p, tot_corr_occafternocc_p
   REAL(8)  :: tot_corr_noccafterocc, tot_corr_noccafternocc, tot_corr_noccafterocc_y, tot_corr_noccafternocc_y, tot_corr_noccafterocc_p, tot_corr_noccafternocc_p

   REAL(8)  :: tot_occafterocc_stu, tot_occafternocc_stu, tot_occafterocc_ltu, tot_occafternocc_ltu
   REAL(8)  :: tot_noccafterocc_stu, tot_noccafternocc_stu, tot_noccafterocc_ltu, tot_noccafternocc_ltu

   REAL(8), DIMENSION(4) :: tot_occafterocc_vector, tot_occafternocc_vector, tot_noccafterocc_vector, tot_noccafternocc_vector
   REAL(8), DIMENSION(4) :: tot_occafterocc_vector_young, tot_occafternocc_vector_young, tot_noccafterocc_vector_young, tot_noccafternocc_vector_young
   REAL(8), DIMENSION(4) :: tot_occafterocc_vector_prime, tot_occafternocc_vector_prime, tot_noccafterocc_vector_prime, tot_noccafternocc_vector_prime
   
   INTEGER, DIMENSION(10,2) :: repeatmobspells_matrix

   REAL(8), DIMENSION(18) :: data_compduration_cocc, data_compduration_nocc
   REAL(8), DIMENSION(18) :: data_compduration_cocc_young, data_compduration_nocc_young
   REAL(8), DIMENSION(18) :: data_compduration_cocc_prime, data_compduration_nocc_prime



   REAL(8)  :: stock_occafterocc, stock_occafternocc, stock_occafterocc_y, stock_occafternocc_y, stock_occafterocc_p, stock_occafternocc_p
   REAL(8)  :: stock_noccafterocc, stock_noccafternocc, stock_noccafterocc_y, stock_noccafternocc_y, stock_noccafterocc_p, stock_noccafternocc_p
   REAL(8)  :: poccafterocc, poccafternocc, poccafterocc_y, poccafternocc_y, poccafterocc_p, poccafternocc_p
   REAL(8)  :: pnoccafterocc, pnoccafternocc, pnoccafterocc_y, pnoccafternocc_y, pnoccafterocc_p, pnoccafternocc_p


   REAL(8), DIMENSION(2)  :: stock_occafterocc_uspell, stock_occafternocc_uspell, stock_occafterocc_y_uspell, stock_occafternocc_y_uspell, stock_occafterocc_p_uspell, stock_occafternocc_p_uspell
   REAL(8), DIMENSION(2)  :: stock_noccafterocc_uspell, stock_noccafternocc_uspell, stock_noccafterocc_y_uspell, stock_noccafternocc_y_uspell, stock_noccafterocc_p_uspell, stock_noccafternocc_p_uspell
   REAL(8), DIMENSION(2)  :: poccafterocc_uspell, poccafternocc_uspell, poccafterocc_y_uspell, poccafternocc_y_uspell, poccafterocc_p_uspell, poccafternocc_p_uspell
   REAL(8), DIMENSION(2)  :: pnoccafterocc_uspell, pnoccafternocc_uspell, pnoccafterocc_y_uspell, pnoccafternocc_y_uspell, pnoccafterocc_p_uspell, pnoccafternocc_p_uspell
   REAL(8), DIMENSION(2)  :: noccafterocc_uspell, noccafternocc_uspell, occafterocc_uspell, occafternocc_uspell


   INTEGER :: uspellength_ind, prev_empl_ind
   INTEGER(4), DIMENSION(tmax_sim2) :: age_sim, occ_change_before_ind, occ_change_sim
                                            ! occ_change_sim (0 can't tell, 1 yes, 2 no)




   REAL(8) :: unemp_ave, unemp_ave2, prod_ave, prod_ave2, wage_ave, wage_ave2, reall_ave, reall_ave2, uadj18m_ave, unemp_earlier_e_ave, uall_ave, uall_yrave
   REAL(8) :: sepyoung_ave, sepprime_ave
   REAL(8) :: jf_ave, jf_ave2, jfo_ave, sep_ave, sep_ave2, v_ave, v_ave2, theta_ave, theta_ave2, z_ave, reall_ave_conditional, sep_yrave, sep_y_yrave, sep_p_yrave
   INTEGER :: jf_ave2_counter
   REAL(8) :: sep_ave_1m, sep_ave_2m, sep_ave_3m,sep_ave_4m, sep_ave_5m, sep_ave_6m, sep_ave_7m, sep_ave_8m, sep_ave_9m, sep_ave_10m, sep_ave_11m, sep_ave_12m
   REAL(8) :: udur_occ_ave, udur_nocc_ave, udur_occ_ave_q, udur_nocc_ave_q
   REAL(8) :: udur_occ_young_ave, udur_nocc_young_ave, udur_occ_young_ave_q, udur_nocc_young_ave_q 
   REAL(8) :: udur_occ_prime_ave, udur_nocc_prime_ave, udur_occ_prime_ave_q, udur_nocc_prime_ave_q
   REAL(8), DIMENSION(12) :: udur_mvec_occ_ave, udur_mvec_occ_with_u, udur_mvec_occ_with_hpu !, udur_mvec_occ_with_hplu, udur_mvec_occ_with_lu
   REAL(8), DIMENSION(12) :: udur_mvec_occ_young_ave,  udur_mvec_occ_young_with_u, udur_mvec_occ_young_with_hpu !, udur_mvec_occ_young_with_hplu, udur_mvec_occ_young_with_lu
   REAL(8), DIMENSION(12) :: udur_mvec_occ_prime_ave,  udur_mvec_occ_prime_with_u, udur_mvec_occ_prime_with_hpu !, udur_mvec_occ_prime_with_hplu, udur_mvec_occ_prime_with_lu
   REAL(8), DIMENSION(12) :: udur_mvec_nocc_ave,  udur_mvec_nocc_with_u, udur_mvec_nocc_with_hpu !, udur_mvec_nocc_with_hplu, udur_mvec_nocc_with_lu
   REAL(8), DIMENSION(12) :: udur_mvec_nocc_young_ave, udur_mvec_nocc_young_with_u, udur_mvec_nocc_young_with_hpu !, udur_mvec_nocc_young_with_hplu, udur_mvec_nocc_young_with_lu
   REAL(8), DIMENSION(12) :: udur_mvec_nocc_prime_ave, udur_mvec_nocc_prime_with_u, udur_mvec_nocc_prime_with_hpu !, udur_mvec_nocc_prime_with_hplu, udur_mvec_nocc_prime_with_lu
   REAL(8) :: udur_occ_with_hplu, udur_occ_with_lu, udur_occ_with_u, udur_nocc_with_u, udur_occ_with_hpu, udur_nocc_with_hpu
   REAL(8) :: udur_nocc_with_hplu, udur_nocc_with_lu
   REAL(8) :: udur_occ_young_with_hplu, udur_occ_young_with_lu, udur_occ_young_with_u, udur_nocc_young_with_u, udur_occ_young_with_hpu, udur_nocc_young_with_hpu
   REAL(8) :: udur_nocc_young_with_hplu, udur_nocc_young_with_lu
   REAL(8) :: udur_nocc_prime_with_hplu, udur_nocc_prime_with_lu, udur_occ_prime_with_u, udur_nocc_prime_with_u, udur_occ_prime_with_hpu, udur_nocc_prime_with_hpu
   REAL(8) :: udur_occ_prime_with_hplu, udur_occ_prime_with_lu
   
   REAL(8) :: sep_ave_1m_posthire, sep_ave_2m_posthire, sep_ave_3m_posthire,sep_ave_4m_posthire, sep_ave_5m_posthire, sep_ave_6m_posthire, sep_ave_7m_posthire, sep_ave_8m_posthire, sep_ave_9m_posthire, sep_ave_10m_posthire, sep_ave_11m_posthire, sep_ave_12m_posthire
   INTEGER :: sep_ave_m_counter, sep_ave_m_posthire_counter, earlier_e_counter, young_earlier_e_counter, prime_earlier_e_counter
   REAL(8) :: u_ave_q, v_ave_q, theta_ave_q, sep_ave_q, jf_ave_q, wage_ave_q, prod_ave_q, reall_ave_q, jfo_ave_q, uall_ave_q
   REAL(8) :: u_ave_q5, v_ave_q5, theta_ave_q5, sep_ave_q5, jf_ave_q5, wage_ave_q5, prod_ave_q5, reall_ave_q5, jfo_ave_q5, uall_ave_q5


   INTEGER(4) :: tempint_u, tempint_jf, tempint_sep, tempint_reall, tempint_jfo
   REAL(8) :: tempreal_wage, tempreal_v, tempreal_prod, tempint_e, testreal, testreal2, testreal3

   REAL(8) :: cocc_ave, cocc_young_ave, cocc_prime_ave, jfocc_ave, jfocc_young_ave, jfocc_prime_ave
   REAL(8) :: nocc_ave, nocc_young_ave, nocc_prime_ave, jfnocc_ave, jfnocc_young_ave, jfnocc_prime_ave
   REAL(8) :: cocc_inflow_ave, cocc_young_inflow_ave, cocc_prime_inflow_ave
   REAL(8) :: nocc_inflow_ave, nocc_young_inflow_ave, nocc_prime_inflow_ave
   REAL(8) :: u_young_ave, u_prime_ave, uadj18m_young_ave, uadj18m_prime_ave, u_earlier_e_young_ave, u_earlier_e_prime_ave
   REAL(8) :: noccafternocc_ave, noccafterocc_ave, noccafternocc_y_ave, noccafterocc_y_ave, noccafternocc_p_ave, noccafterocc_p_ave
   REAL(8) :: noccafterall_ave, noccafterall_y_ave, noccafterall_p_ave
   REAL(8) :: hpu_p33, hpu_p50, hpu_p67

   REAL(8) :: jf_young_ave, jf_prime_ave

   ! SEPARATIONS
   REAL(8) :: sepocc_ave, sepnocc_ave, sepocc_y_ave, sepnocc_y_ave, sepocc_p_ave, sepnocc_p_ave
   REAL(8) :: sepocc_ave_counter, sepnocc_ave_counter, sepocc_y_ave_counter, sepnocc_y_ave_counter, sepocc_p_ave_counter, sepnocc_p_ave_counter

    REAL(8)  :: sepocc_afternocc_2_5yr, sepocc_afternocc_2_5yr_2m, sepocc_afternocc_3yr
    REAL(8) :: sep_afternocc_2_5yr, sep_afternocc_2_5yr_2m
    REAL(8) :: sepnocc_afternocc_2_5yr, sepnocc_afternocc_2_5yr_2m
    INTEGER :: sepocc_afternocc_2_5yr_counter, sepocc_afternocc_3yr_counter, sep_afternocc_2_5yr_counter, sepnocc_afternocc_2_5yr_counter


    ! BUSINESS CYCLE ELASTICITIES
   REAL(8) :: elmatching_ave, elmatching_lindt_ave, el_u_with_v_lindt, el_u_with_p_lindt,el_u_with_p
   REAL(8) :: el_u_with_v, el_u5_with_v5, el_v_with_u, el_v5_with_u5
   REAL(8) :: unemp_prod_elasticity, u_young_prod_elasticity, u_prime_prod_elasticity, sep_young_prod_elasticity, sep_prime_prod_elasticity
   REAL(8) :: cocc_inflow_prod_elasticity, cocc_inflow_unemp_elasticity, jf_young_prod_elasticity, jf_prime_prod_elasticity, jf_prod_elasticity, sep_prod_elasticity
   REAL(8) :: cocc_young_inflow_prod_elasticity, cocc_prime_inflow_prod_elasticity, cocc_young_inflow_unemp_elasticity, cocc_prime_inflow_unemp_elasticity
   REAL(8) :: sep_young_unemp_elasticity, sep_prime_unemp_elasticity, wage_prod_elasticity, jfocc_prod_elasticity, jfnocc_prod_elasticity
   REAL(8) :: jfocc5_prod_elasticity, jfnocc5_prod_elasticity, jf5_young_prod_elasticity, jf5_prime_prod_elasticity, jf5_prod_elasticity
   REAL(8) :: jfocc5_prod_semielasticity, jfnocc5_prod_semielasticity, jf5_young_prod_semielasticity, jf5_prime_prod_semielasticity, jf5_prod_semielasticity
   REAL(8) :: jfocc5_unemp_semielasticity, jfnocc5_unemp_semielasticity, jf5_young_unemp_semielasticity, jf5_prime_unemp_semielasticity, jf5_unemp_semielasticity
   REAL(8) :: jfocc5_unemp_elasticity, jfnocc5_unemp_elasticity, jf5_young_unemp_elasticity, jf5_prime_unemp_elasticity, jf5_unemp_elasticity

   REAL(8) :: jfocc_unemp_elasticity, jfnocc_unemp_elasticity
   REAL(8) :: jf_young_unemp_elasticity, jf_prime_unemp_elasticity, jf_unemp_elasticity, sep_unemp_elasticity
   REAL(8) :: sep_young_prod_semielasticity, sep_prod_semielasticity, sep_prime_prod_semielasticity, jfocc_prod_semielasticity, jfnocc_prod_semielasticity
   REAL(8) :: jf_prod_semielasticity, jf_young_prod_semielasticity, jf_prime_prod_semielasticity
   REAL(8) :: sep_young_unemp_semielasticity, sep_unemp_semielasticity, sep_prime_unemp_semielasticity, jfocc_unemp_semielasticity, jfnocc_unemp_semielasticity
   REAL(8) :: jf_unemp_semielasticity, jf_young_unemp_semielasticity, jf_prime_unemp_semielasticity
    ! u-durations (in wks)
   !REAL(8) :: u5w_prod_elasticity, u15w_prod_elasticity, u27w_prod_elasticity, ugt27w_prod_elasticity
   !REAL(8) :: u5w_prod_semielasticity, u15w_prod_semielasticity, u27w_prod_semielasticity, ugt27w_prod_semielasticity
   REAL(8) :: u5w_unemp_elasticity, u15w_unemp_elasticity, u27w_unemp_elasticity, ugt27w_unemp_elasticity
   REAL(8) :: u5w_unemp_semielasticity, u15w_unemp_semielasticity, u27w_unemp_semielasticity, ugt27w_unemp_semielasticity
   !REAL(8) :: uprop5w_prod_elasticity, uprop15w_prod_elasticity, uprop27w_prod_elasticity, upropgt27w_prod_elasticity
   !REAL(8) :: uprop5w_prod_semielasticity, uprop15w_prod_semielasticity, uprop27w_prod_semielasticity, upropgt27w_prod_semielasticity
   !REAL(8) :: uprop5w_unemp_elasticity, uprop15w_unemp_elasticity, uprop27w_unemp_elasticity, upropgt27w_unemp_elasticity
   !REAL(8) :: uprop5w_unemp_semielasticity, uprop15w_unemp_semielasticity, uprop27w_unemp_semielasticity, upropgt27w_unemp_semielasticity
   !REAL(8) :: luprop5w_prod_elasticity, luprop15w_prod_elasticity, luprop27w_prod_elasticity, lupropgt27w_prod_elasticity
   !REAL(8) :: luprop5w_prod_semielasticity, luprop15w_prod_semielasticity, luprop27w_prod_semielasticity, lupropgt27w_prod_semielasticity
   !REAL(8) :: luprop5w_unemp_elasticity, luprop15w_unemp_elasticity, luprop27w_unemp_elasticity, lupropgt27w_unemp_elasticity
   !REAL(8) :: luprop5w_unemp_semielasticity, luprop15w_unemp_semielasticity, luprop27w_unemp_semielasticity, lupropgt27w_unemp_semielasticity
   REAL(8) :: hp_uprop5w_prod_elasticity, hp_uprop15w_prod_elasticity, hp_uprop27w_prod_elasticity, hp_upropgt27w_prod_elasticity
   REAL(8) :: hp_uprop5w_prod_semielasticity, hp_uprop15w_prod_semielasticity, hp_uprop27w_prod_semielasticity, hp_upropgt27w_prod_semielasticity
   REAL(8) :: hp_uprop5w_unemp_elasticity, hp_uprop15w_unemp_elasticity, hp_uprop27w_unemp_elasticity, hp_upropgt27w_unemp_elasticity
   REAL(8) :: hp_uprop5w_unemp_semielasticity, hp_uprop15w_unemp_semielasticity, hp_uprop27w_unemp_semielasticity, hp_upropgt27w_unemp_semielasticity
   !REAL(8) :: hpl_uprop5w_prod_elasticity, hpl_uprop15w_prod_elasticity, hpl_uprop27w_prod_elasticity, hpl_upropgt27w_prod_elasticity
   !REAL(8) :: hpl_uprop5w_prod_semielasticity, hpl_uprop15w_prod_semielasticity, hpl_uprop27w_prod_semielasticity, hpl_upropgt27w_prod_semielasticity
   !REAL(8) :: hpl_uprop5w_unemp_elasticity, hpl_uprop15w_unemp_elasticity, hpl_uprop27w_unemp_elasticity, hpl_upropgt27w_unemp_elasticity
   !REAL(8) :: hpl_uprop5w_unemp_semielasticity, hpl_uprop15w_unemp_semielasticity, hpl_uprop27w_unemp_semielasticity, hpl_upropgt27w_unemp_semielasticity

    ! u-durations (in wks)
   !REAL(8) :: u3m_prod_elasticity, u5m_prod_elasticity, u9m_prod_elasticity, u13m_prod_elasticity, ugt13m_prod_elasticity
   !REAL(8) :: u3m_prod_semielasticity, u5m_prod_semielasticity, u9m_prod_semielasticity, u13m_prod_semielasticity, ugt13m_prod_semielasticity
   REAL(8) :: u3m_unemp_elasticity, u5m_unemp_elasticity, u9m_unemp_elasticity, u13m_unemp_elasticity, ug13m_unemp_elasticity
   REAL(8) :: u3m_unemp_semielasticity, u5m_unemp_semielasticity, u9m_unemp_semielasticity, u13m_unemp_semielasticity, ug13m_unemp_semielasticity
   REAL(8) :: u3m_unemp_yng_elasticity, u5m_unemp_yng_elasticity, u9m_unemp_yng_elasticity, u13m_unemp_yng_elasticity, ug13m_unemp_yng_elasticity
   REAL(8) :: u3m_unemp_yng_semielasticity, u5m_unemp_yng_semielasticity, u9m_unemp_yng_semielasticity, u13m_unemp_yng_semielasticity, ug13m_unemp_yng_semielasticity
   REAL(8) :: u3m_unemp_prm_elasticity, u5m_unemp_prm_elasticity, u9m_unemp_prm_elasticity, u13m_unemp_prm_elasticity, ug13m_unemp_prm_elasticity
   REAL(8) :: u3m_unemp_prm_semielasticity, u5m_unemp_prm_semielasticity, u9m_unemp_prm_semielasticity, u13m_unemp_prm_semielasticity, ug13m_unemp_prm_semielasticity


   !REAL(8) :: uprop3m_prod_elasticity, uprop5m_prod_elasticity, uprop9m_prod_elasticity, uprop13m_prod_elasticity, upropg13m_prod_elasticity
   !REAL(8) :: uprop3m_prod_semielasticity, uprop5m_prod_semielasticity, uprop9m_prod_semielasticity, uprop13m_prod_semielasticity, upropg13m_prod_semielasticity
   !REAL(8) :: uprop3m_unemp_elasticity, uprop5m_unemp_elasticity, uprop9m_unemp_elasticity, uprop13m_unemp_elasticity, upropg13m_unemp_elasticity
   !REAL(8) :: uprop3m_unemp_semielasticity, uprop5m_unemp_semielasticity, uprop9m_unemp_semielasticity, uprop13m_unemp_semielasticity, upropg13m_unemp_semielasticity
   !REAL(8) :: luprop3m_prod_elasticity, luprop5m_prod_elasticity, luprop9m_prod_elasticity, luprop13m_prod_elasticity, lupropg13m_prod_elasticity
   !REAL(8) :: luprop3m_prod_semielasticity, luprop5m_prod_semielasticity, luprop9m_prod_semielasticity, luprop13m_prod_semielasticity, lupropg13m_prod_semielasticity
   !REAL(8) :: luprop3m_unemp_elasticity, luprop5m_unemp_elasticity, luprop9m_unemp_elasticity, luprop13m_unemp_elasticity, lupropg13m_unemp_elasticity
   !REAL(8) :: luprop3m_unemp_semielasticity, luprop5m_unemp_semielasticity, luprop9m_unemp_semielasticity, luprop13m_unemp_semielasticity, lupropg13m_unemp_semielasticity
   REAL(8) :: hp_uprop3m_prod_elasticity, hp_uprop5m_prod_elasticity, hp_uprop9m_prod_elasticity, hp_uprop13m_prod_elasticity,hp_upropg13m_prod_elasticity
   REAL(8) :: hp_uprop3m_prod_semielasticity, hp_uprop5m_prod_semielasticity, hp_uprop9m_prod_semielasticity, hp_uprop13m_prod_semielasticity, hp_upropg13m_prod_semielasticity
   REAL(8) :: hp_uprop3m_unemp_elasticity, hp_uprop5m_unemp_elasticity, hp_uprop9m_unemp_elasticity, hp_uprop13m_unemp_elasticity, hp_upropg13m_unemp_elasticity
   REAL(8) :: hp_sm_uprop3m_unemp_elasticity, hp_sm_uprop5m_unemp_elasticity, hp_sm_uprop9m_unemp_elasticity, hp_sm_uprop13m_unemp_elasticity, hp_sm_upropg13m_unemp_elasticity
   REAL(8) :: hp_uprop3m_unemp_semielasticity, hp_uprop5m_unemp_semielasticity, hp_uprop9m_unemp_semielasticity, hp_uprop13m_unemp_semielasticity, hp_upropg13m_unemp_semielasticity
   REAL(8) :: hp_uprop3m_prod_yng_elasticity, hp_uprop5m_prod_yng_elasticity, hp_uprop9m_prod_yng_elasticity, hp_uprop13m_prod_yng_elasticity,hp_upropg13m_prod_yng_elasticity
   REAL(8) :: hp_uprop3m_prod_yng_semielasticity, hp_uprop5m_prod_yng_semielasticity, hp_uprop9m_prod_yng_semielasticity, hp_uprop13m_prod_yng_semielasticity, hp_upropg13m_prod_yng_semielasticity
   REAL(8) :: hp_uprop3m_unemp_yng_elasticity, hp_uprop5m_unemp_yng_elasticity, hp_uprop9m_unemp_yng_elasticity, hp_uprop13m_unemp_yng_elasticity, hp_upropg13m_unemp_yng_elasticity
   REAL(8) :: hp_uprop3m_unemp_yng_semielasticity, hp_uprop5m_unemp_yng_semielasticity, hp_uprop9m_unemp_yng_semielasticity, hp_uprop13m_unemp_yng_semielasticity, hp_upropg13m_unemp_yng_semielasticity
   REAL(8) :: hp_uprop3m_prod_prm_elasticity, hp_uprop5m_prod_prm_elasticity, hp_uprop9m_prod_prm_elasticity, hp_uprop13m_prod_prm_elasticity,hp_upropg13m_prod_prm_elasticity
   REAL(8) :: hp_uprop3m_prod_prm_semielasticity, hp_uprop5m_prod_prm_semielasticity, hp_uprop9m_prod_prm_semielasticity, hp_uprop13m_prod_prm_semielasticity, hp_upropg13m_prod_prm_semielasticity
   REAL(8) :: hp_uprop3m_unemp_prm_elasticity, hp_uprop5m_unemp_prm_elasticity, hp_uprop9m_unemp_prm_elasticity, hp_uprop13m_unemp_prm_elasticity, hp_upropg13m_unemp_prm_elasticity
   REAL(8) :: hp_uprop3m_unemp_prm_semielasticity, hp_uprop5m_unemp_prm_semielasticity, hp_uprop9m_unemp_prm_semielasticity, hp_uprop13m_unemp_prm_semielasticity, hp_upropg13m_unemp_prm_semielasticity

   REAL(8) :: sm_hp_uprop3m_u_semi_el, sm_hp_uprop5m_u_semi_el, sm_hp_uprop9m_u_semi_el, sm_hp_uprop13m_u_semi_el, sm_hp_upropg13m_u_semi_el
   !REAL(8) :: hpl_uprop3m_prod_elasticity, hpl_uprop5m_prod_elasticity, hpl_uprop9m_prod_elasticity, hpl_uprop13m_prod_elasticity, hpl_upropg13m_prod_elasticity
   !REAL(8) :: hpl_uprop3m_prod_semielasticity, hpl_uprop5m_prod_semielasticity, hpl_uprop9m_prod_semielasticity, hpl_uprop13m_prod_semielasticity, hpl_upropg13m_prod_semielasticity
   !REAL(8) :: hpl_uprop3m_unemp_elasticity, hpl_uprop5m_unemp_elasticity, hpl_uprop9m_unemp_elasticity, hpl_uprop13m_unemp_elasticity, hpl_upropg13m_unemp_elasticity
   !REAL(8) :: hpl_uprop3m_unemp_semielasticity, hpl_uprop5m_unemp_semielasticity, hpl_uprop9m_unemp_semielasticity, hpl_uprop13m_unemp_semielasticity, hpl_upropg13m_unemp_semielasticity

   REAL(8) :: cycl_sep_age_shift_muellermom
   REAL(8) :: cycl_jf_age_shift_muellermom, jf_y_yrave, jf_p_yrave
   REAL(8) :: cycl_u_age_shift_muellermom, u_y_yrave, u_p_yrave

   REAL(8) :: cocc_outflow_prod_elasticity, cocc_young_outflow_prod_elasticity, cocc_prime_outflow_prod_elasticity
   REAL(8) :: cocc_outflow_unemp_elasticity, cocc_young_outflow_unemp_elasticity, cocc_prime_outflow_unemp_elasticity
   REAL(8) :: cocc_outflow_prod_semielasticity, cocc_young_outflow_prod_semielasticity, cocc_prime_outflow_prod_semielasticity
   REAL(8) :: cocc_outflow_unemp_semielasticity, cocc_young_outflow_unemp_semielasticity, cocc_prime_outflow_unemp_semielasticity

   REAL(8) :: var_jfocc, var_jfnocc

   REAL(8) :: uprop_3m_ave, uprop_5m_ave, uprop_9m_ave, uprop_13m_ave, uprop_g13m_ave
   REAL(8) :: var_uprop_3m_ave, var_uprop_5m_ave, var_uprop_9m_ave, var_uprop_13m_ave, var_uprop_g13m_ave
   REAL(8) :: hp_var_uprop_3m_ave, hp_var_uprop_5m_ave, hp_var_uprop_9m_ave, hp_var_uprop_13m_ave, hp_var_uprop_g13m_ave
   REAL(8) :: var_l_uprop_3m_ave, var_l_uprop_5m_ave, var_l_uprop_9m_ave, var_l_uprop_13m_ave, var_l_uprop_g13m_ave
   REAL(8) :: hp_var_l_uprop_3m_ave, hp_var_l_uprop_5m_ave, hp_var_l_uprop_9m_ave, hp_var_l_uprop_13m_ave, hp_var_l_uprop_g13m_ave

   REAL(8) :: uprop_yng_3m_ave, uprop_yng_5m_ave, uprop_yng_9m_ave, uprop_yng_13m_ave, uprop_yng_g13m_ave
   REAL(8) :: var_uprop_yng_3m_ave, var_uprop_yng_5m_ave, var_uprop_yng_9m_ave, var_uprop_yng_13m_ave, var_uprop_yng_g13m_ave
   REAL(8) :: hp_var_uprop_yng_3m_ave, hp_var_uprop_yng_5m_ave, hp_var_uprop_yng_9m_ave, hp_var_uprop_yng_13m_ave, hp_var_uprop_yng_g13m_ave
   REAL(8) :: var_l_uprop_yng_3m_ave, var_l_uprop_yng_5m_ave, var_l_uprop_yng_9m_ave, var_l_uprop_yng_13m_ave, var_l_uprop_yng_g13m_ave
   REAL(8) :: hp_var_l_uprop_yng_3m_ave, hp_var_l_uprop_yng_5m_ave, hp_var_l_uprop_yng_9m_ave, hp_var_l_uprop_yng_13m_ave, hp_var_l_uprop_yng_g13m_ave

   REAL(8) :: uprop_prm_3m_ave, uprop_prm_5m_ave, uprop_prm_9m_ave, uprop_prm_13m_ave, uprop_prm_g13m_ave
   REAL(8) :: var_uprop_prm_3m_ave, var_uprop_prm_5m_ave, var_uprop_prm_9m_ave, var_uprop_prm_13m_ave, var_uprop_prm_g13m_ave
   REAL(8) :: hp_var_uprop_prm_3m_ave, hp_var_uprop_prm_5m_ave, hp_var_uprop_prm_9m_ave, hp_var_uprop_prm_13m_ave, hp_var_uprop_prm_g13m_ave
   REAL(8) :: var_l_uprop_prm_3m_ave, var_l_uprop_prm_5m_ave, var_l_uprop_prm_9m_ave, var_l_uprop_prm_13m_ave, var_l_uprop_prm_g13m_ave
   REAL(8) :: hp_var_l_uprop_prm_3m_ave, hp_var_l_uprop_prm_5m_ave, hp_var_l_uprop_prm_9m_ave, hp_var_l_uprop_prm_13m_ave, hp_var_l_uprop_prm_g13m_ave


   REAL(8) :: ult5wk_ave, u5lt15wk_ave, u15lt27wk_ave, ugt27wk_ave
   REAL(8) :: var_ult5wk_ave, var_u5lt15wk_ave, var_u15lt27wk_ave, var_ugt27wk_ave
   REAL(8) :: var_l_ult5wk_ave, var_l_u5lt15wk_ave, var_l_u15lt27wk_ave, var_l_ugt27wk_ave



    REAL(8), DIMENSION(ppts) :: u_prod_relation, prod_distribution
    REAL(8), DIMENSION(ppts) :: prod_median, wage_median, sep_prod_above_med, sep_prod_below_med, sep_wage_above_med, sep_wage_below_med

    REAL(8), ALLOCATABLE, DIMENSION(:,:) :: hp_scratch


    !Target and non-target (result) moments
    REAL(8):: var_u, var_v, var_theta, var_sep, var_jf, var_jfo, var_wage, var_prod, var_reall, var_u5, var_v5, var_theta5, &
                                        var_sep5, var_jf5, var_jfo5, var_wage5, var_prod5, var_reall5, var_reall5_2, var_uall, var_uall5
    REAL(8):: corr_uv, corr_uv_lindt, corr_utheta, corr_usep, corr_ujf, corr_uwage, corr_uprod, corr_ureall, corr_vtheta,&
                corr_uallv, corr_uallv_lindt, corr_ualltheta, corr_uallsep, corr_ualljf, corr_uallwage, corr_uallprod, corr_uallreall, corr_uallu, &
                                 corr_vsep, corr_vjf, corr_vwage, corr_vprd, corr_vreall, &
                                 corr_thetasep, corr_thetajf, corr_thetawage, corr_thetaprod, corr_thetareall, &
                                 corr_sepjf, corr_sepwage, corr_sepprod, corr_sepreall, &
                                 corr_jfwage, corr_jfprod, corr_wageprod, corr_wagereall, corr_prodreall, &
                                 corr_jfreall, corr_vprod, corr_ujfo, corr_vjfo, corr_thetajfo, corr_sepjfo, corr_jfjfo, &
                                 corr_wagejfo, corr_prodjfo, corr_realljfo, &
                                 corr_uv5, corr_uv5_2, corr_utheta5, corr_usep5, corr_ujf5, corr_uwage5, corr_uprod5, corr_ureall5, corr_vtheta5,&
                                 corr_uallu5, corr_uallv5, corr_uallv5_2, corr_ualltheta5, corr_uallsep5, corr_ualljf5, corr_uallwage5, corr_uallprod5, corr_uallreall5, &
                                 corr_vsep5, corr_vjf5, corr_vwage5, corr_vprd5, corr_vreall5, &
                                 corr_thetasep5, corr_thetajf5, corr_thetawage5, corr_thetaprod5, corr_thetareall5, &
                                 corr_sepjf5, corr_sepwage5, corr_sepprod5, corr_sepreall5, &
                                 corr_jfwage5, corr_jfprod5, corr_wageprod5, corr_wagereall5, corr_prodreall5, &
                                 corr_jfreall5, corr_vprod5, corr_ujfo5, corr_vjfo5, corr_thetajfo5, corr_sepjfo5, corr_jfjfo5, &
                                 corr_wagejfo5, corr_prodjfo5, corr_realljfo5

    REAL(8) :: ac_u, ac_v, ac_theta, ac_sep, ac_jf, ac_wage, ac_prod, ac_reall, ac_jfo, ac_u5, &
                    ac_v5, ac_theta5, ac_sep5, ac_jf5, ac_wage5, ac_prod5, ac_reall5, ac_jfo5, ac_uall5, ac_uall


    REAL(8) :: var_uyoung, var_uprime, var_jfyoung, var_jfprime, var_sepyoung, var_sepprime, var_cmyoung,var_cmprime, var_cm, var_lsepyoung, var_lsepprime
    REAL(8) :: corr_uyoungprod, corr_uprimeprod, corr_jfyoungprod, corr_jfprimeprod, corr_sepyoungprod, corr_sepprimeprod
    REAL(8) :: corr_cmyoungprod, corr_cmprimeprod, corr_jfyoungunempl, corr_jfprimeunempl, corr_sepyoungunempl, corr_sepprimeunempl, corr_cmyoungunempl, corr_cmprimeunempl

    REAL(8) :: relstdev_occmob_y


    !REAL(8), DIMENSION(tmax) :: w_ave
    !REAL(8), DIMENSION(tmax-1) :: w_ave_uj, w_chg_jj, w_chg_uj
    !REAL(8), DIMENSION(nsim, tmax) ::  w_chg, w_chg_u


    REAL(8), ALLOCATABLE, DIMENSION(:,:) :: Xjf,Yjf
    !REAL(8)  ::
    REAL(8), DIMENSION(2) :: betajf

    INTEGER :: tmax_swindow_qtr



    REAL(8), DIMENSION(12) :: tmom             ! total moments
   ! statistics to report
    INTEGER(4) :: ycnt, vvcnt, vvpts, ttemp, ttemp2, vtemp, xtemp, ztemp, ztemp_indicator, tempstep, counter1, tempcounter, &
                                                                            vvcnt2, vvcnt3,  tempcounter1, tempcounter2, tempcounter3, sim_id, zcnt2, exper, yauxcnt,&
                                                                           expertemp, counter_eu, counter_en, counter_ne, counter_nu, counter_ue, counter_un, counter2, counter3
    INTEGER(4) :: yearcnt, ncnt
    INTEGER(4) :: truthflag, truthflag2, flag1, flag2, flag3,switch_ind, u_flag, e_flag, flag_windowsurvival
    REAL(8) :: realstep, tempreal,tempreal2, tempreal3, factor, thetatemp
    REAL(8) :: tempreal4, tempreal5, tempreal6


    ! i/o, and time
    REAL(8)                 :: mu_temp, binsize, xstep, xvalue
    INTEGER(4)              :: bincounter, flag_z, i1
    LOGICAL                 :: exists, file_exists
    CHARACTER*24            :: dt
    CHARACTER*12, DIMENSION(3) :: time_string, input_string, time_string_begin
    REAL                    :: t1, t2
    CHARACTER*24            :: filename, x1,x2, errorpoint
    INTEGER 				:: ierr
    CHARACTER*24            :: filenm, filenm2
    CHARACTER*10            :: mom_id



! BASIC MATRICES!!!!!!!!!

  REAL(8), DIMENSION(ppts,xpts, zpts) :: prod
REAL(8), DIMENSION(ppts) :: pvector         !productivity
REAL(8), DIMENSION(ppts, ppts) :: ptransmatrix
REAL(8), DIMENSION(ppts) :: pnewpdf             ! unconditional z distribution for new draw
REAL(8), DIMENSION(xpts) :: xvector
REAL(8), DIMENSION(xpts) :: xpdfvector, xcdfvector
REAL(8), DIMENSION(xpts, xpts) ::  xtransmatrix
REAL(8), DIMENSION(zpts) :: zvector              ! dimension zpts
REAL(8), DIMENSION(zpts,zpts) ::  ztransmatrix   ! Probabilities of the idiosyncratic shock, dimension zpts
REAL(8), DIMENSION(zpts) :: znewpdf, znewcdf             ! unconditional z distribution for new draw
REAL(8), DIMENSION(ppts) :: temp_p_cdf

! tmax has the function here of having both present and tomorrow's value. Also used to figure out convergence!
REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: TH       ! (ppts,  xpts, zpts, tmax)          Tightness in market
REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: DmaxU     ! (ppts, xpts, zpts,tmax)                 Expected surplus value of search for unemployed
!REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: EV       ! (ppts, zpts,tmax)                 Expected value of a new match
REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: Wage         ! (ppts, zpts,tmax)        Expected productivity of the match
!REAL(8), ALLOCATABLE, DIMENSION(:,:,:) :: phi     ! (ppts, zpts,tmax)                 piece rate offered to previously unemployed
REAL(8), ALLOCATABLE, DIMENSION(:)  :: Realloc  !(ppts, tmax)              the value of reallocating


!policy functions (matrices)for the WORKERS
INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: poliDW ! (ppts, xpts, zpts, tmax)        MATCH BREAKUP DECISIONS: 1 for CONTINUATION, 0 for BREAKUP
INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: poliR ! (ppts, xpts, zpts, tmax)                 Reallocation decisions: 1 for CONTINUATION, 0 for REALLOCATION
!-------------------------
!  AUXILIARY FUNCTIONS
!-------------------------
REAL(8), ALLOCATABLE, DIMENSION(:,:,:)   :: JF       ! (ypts, xpts, zpts,  tmax)   Prob of Succesful application *from* a match unemployment at t





REAL, DIMENSION(zpts) :: z_restricted_pdf, z_restricted_cdf, zoldpdf, zoldcdf
INTEGER, DIMENSION (zpts) :: best_islands_locator
INTEGER :: max_chosen_islands, islandcounter2, zcounter1


INTEGER, DIMENSION(ppts, xpts) :: reservation_zr, reservation_zq, zr_res, zs_res
INTEGER, DIMENSION(xpts) :: sepcutoff, reallcutoff

    !=============================================
    ! CONSTANTS TO CONTROL OPTIONS IN PROGRAM
    !=============================================


     INTEGER :: loopcounter
    INTEGER :: runnumber, threadnumber, islandcounter, islandcounter1
    INTEGER :: expts

    INTEGER(4) :: youngcutoff, lowcutoff, highcutoff, highcutoff2, gencutoff


    ! ------------------------------------
    !  FURTHER DISTRIBUTIONS
    !-----------------------------------------------

    ! distribution of aggregate states
    REAL(8), DIMENSION(ppts) :: p_distribution_sim

    ! distribution of (employed and )unemployed with aggregate states
    REAL(8), DIMENSION(ppts) :: up_distribution_sim, up_distribution_sim2, ep_distribution_sim, &
                                        uallp_distribution_sim, eallp_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_young_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_prime_distribution_sim
    ! distribution of (employed and unemployed over islands with different aggregate states
    REAL(8), DIMENSION(ppts, zpts) :: upz_distribution_sim, epz_distribution_sim
    REAL(8), DIMENSION(ppts, zpts) :: upz_young_distribution_sim, epz_young_distribution_sim
    REAL(8), DIMENSION(ppts, zpts) :: upz_prime_distribution_sim, epz_prime_distribution_sim
    ! total rest, reallocation, search unemployment
    ! distribution of (employed and )unemployed with aggregate states
    REAL(8), DIMENSION(ppts) :: up_rest_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_young_rest_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_prime_rest_distribution_sim
    ! distribution of (employed and )unemployed with aggregate states
    REAL(8), DIMENSION(ppts) :: up_search_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_young_search_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_prime_search_distribution_sim
    ! distribution of (employed and )unemployed with aggregate states
    REAL(8), DIMENSION(ppts) :: up_reall_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_young_reall_distribution_sim
    REAL(8), DIMENSION(ppts) :: up_prime_reall_distribution_sim

    ! for the mM distribution
    ! distribution of (employed and unemployed over islands with different aggregate states
    REAL(8), DIMENSION(ppts, zpts) :: wagepz_distribution_sim, wagepz_young_distribution_sim, wagepz_prime_distribution_sim, &
                                        epz_1yb4_distribution_sim, sep_epz_1yb4_distribution_sim
    REAL(8), DIMENSION(ppts, xpts, zpts) :: epz_x_distribution_sim, epz_x_young_distribution_sim, epz_x_prime_distribution_sim, &
                                            upz_x_distribution_sim, upz_x_young_distribution_sim, upz_x_prime_distribution_sim, &
                                            epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
                                            epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim

    REAL(8) :: epz_x1_pmass, epz_x2_pmass, epz_x3_pmass, epz_x1_young_pmass, epz_x2_young_pmass, epz_x3_young_pmass, &
                                    epz_x1_prime_pmass, epz_x2_prime_pmass, epz_x3_prime_pmass, epz_young_pmass, epz_prime_pmass
    REAL(8), DIMENSION(ppts) :: epz_x1_pmass_p, epz_x2_pmass_p, epz_x3_pmass_p, epz_x1_young_pmass_p, epz_x2_young_pmass_p,&
                                    epz_x3_young_pmass_p, epz_x1_prime_pmass_p, epz_x2_prime_pmass_p, epz_x3_prime_pmass_p, &
                                    epz_young_pmass_p, epz_prime_pmass_p



    !! mM distributions
    REAL(8) :: m0mratio_all, m0mratio_young, m0mratio_prime, m0mratio_x1, m0mratio_x2, m0mratio_x3, m0mratio_age_contr, m0mratio_age_x_contr, m0mratio_age_p_x_contr, m0mratio_age_p_contr
    REAL(8), DIMENSION(ppts) :: m0mratio_all_p, m0mratio_young_p, m0mratio_prime_p, m0mratio_x1_p, m0mratio_x2_p, m0mratio_x3_p
    REAL(8) :: m1mratio_all, m1mratio_young, m1mratio_prime, m1mratio_x1, m1mratio_x2, m1mratio_x3, m1mratio_age_contr, m1mratio_age_x_contr, m1mratio_age_p_x_contr, m1mratio_age_p_contr
    REAL(8), DIMENSION(ppts) :: m1mratio_all_p, m1mratio_young_p, m1mratio_prime_p, m1mratio_x1_p, m1mratio_x2_p, m1mratio_x3_p
    REAL(8) :: m5mratio_all, m5mratio_young, m5mratio_prime, m5mratio_x1, m5mratio_x2, m5mratio_x3, m5mratio_age_contr, m5mratio_age_x_contr, m5mratio_age_p_x_contr, m5mratio_age_p_contr
    REAL(8), DIMENSION(ppts) :: m5mratio_all_p, m5mratio_young_p, m5mratio_prime_p, m5mratio_x1_p, m5mratio_x2_p, m5mratio_x3_p
    REAL(8) :: m10mratio_all, m10mratio_young, m10mratio_prime, m10mratio_x1, m10mratio_x2, m10mratio_x3, m10mratio_age_contr, m10mratio_age_x_contr, m10mratio_age_p_x_contr, m10mratio_age_p_contr
    REAL(8), DIMENSION(ppts) :: m10mratio_all_p, m10mratio_young_p, m10mratio_prime_p, m10mratio_x1_p, m10mratio_x2_p, m10mratio_x3_p
    REAL(8) :: meanwage_all, meanwage_young, meanwage_prime, meanwage_x1, meanwage_x2, meanwage_x3
    REAL(8), DIMENSION(ppts) :: meanwage_all_p, meanwage_young_p, meanwage_prime_p, meanwage_x1_p, meanwage_x2_p, meanwage_x3_p

    REAL(8) :: meanwage_x1_young, meanwage_x2_young, meanwage_x3_young, meanwage_x1_prime, meanwage_x2_prime, meanwage_x3_prime
    REAL(8), DIMENSION(ppts) :: meanwage_x1_young_p, meanwage_x2_young_p, meanwage_x3_young_p, meanwage_x1_prime_p, meanwage_x2_prime_p, meanwage_x3_prime_p
    REAL(8) :: m0mratio_x1_young, m0mratio_x2_young, m0mratio_x3_young, m0mratio_x1_prime, m0mratio_x2_prime, m0mratio_x3_prime
    REAL(8), DIMENSION(ppts) :: m0mratio_x1_young_p, m0mratio_x2_young_p, m0mratio_x3_young_p, m0mratio_x1_prime_p, m0mratio_x2_prime_p, m0mratio_x3_prime_p
    REAL(8) :: m1mratio_x1_young, m1mratio_x2_young, m1mratio_x3_young, m1mratio_x1_prime, m1mratio_x2_prime, m1mratio_x3_prime
    REAL(8), DIMENSION(ppts) :: m1mratio_x1_young_p, m1mratio_x2_young_p, m1mratio_x3_young_p, m1mratio_x1_prime_p, m1mratio_x2_prime_p, m1mratio_x3_prime_p

    REAL(8) :: m5mratio_x1_young, m5mratio_x2_young, m5mratio_x3_young, m5mratio_x1_prime, m5mratio_x2_prime, m5mratio_x3_prime
    REAL(8), DIMENSION(ppts) :: m5mratio_x1_young_p, m5mratio_x2_young_p, m5mratio_x3_young_p, m5mratio_x1_prime_p, m5mratio_x2_prime_p, m5mratio_x3_prime_p
    REAL(8) :: m10mratio_x1_young, m10mratio_x2_young, m10mratio_x3_young, m10mratio_x1_prime, m10mratio_x2_prime, m10mratio_x3_prime
    REAL(8), DIMENSION(ppts) :: m10mratio_x1_young_p, m10mratio_x2_young_p, m10mratio_x3_young_p, m10mratio_x1_prime_p, m10mratio_x2_prime_p, m10mratio_x3_prime_p

    INTEGER :: counter_meanwage_all, counter_meanwage_young, counter_meanwage_prime, counter_meanwage_x1, counter_meanwage_x2, counter_meanwage_x3
    INTEGER, DIMENSION(ppts) :: counter_meanwage_all_p, counter_meanwage_young_p, counter_meanwage_prime_p, counter_meanwage_x1_p, counter_meanwage_x2_p, counter_meanwage_x3_p



    !--------------
    ! FURTHER VECTOR DEFINITIONS
    !------------------------------
    INTEGER, DIMENSION(tmax_sim2/12) :: number_agents               ! keeps track of agents in a growing economy
    INTEGER, DIMENSION(nsim_gen) :: initial_island




    REAL(8) :: reall_moment1, reall_moment2a, reall_moment2b, reall_moment3a, reall_moment3b
    REAL(8) :: duration_moment1, duration_moment2, duration_moment3, duration_moment4, duration_moment5
    REAL(8) :: duration_datamoment1, duration_datamoment2, duration_datamoment3, duration_datamoment4, duration_datamoment5
    REAL(8) :: udur_occ_moment1, udur_occ_moment2, udur_occ_moment3, udur_occ_moment1_data, udur_occ_moment2_data, udur_occ_moment3_data
    REAL(8) :: udur_nocc_moment1, udur_nocc_moment2, udur_nocc_moment3, udur_nocc_moment1_data, udur_nocc_moment2_data, udur_nocc_moment3_data



    REAL(8) :: min_unemp
    REAL(8) :: min_unemp_ee
    REAL(8) :: min_unemp_young_ee
    REAL(8) :: min_unemp_prime_ee

    REAL(8) :: max_unemp
    REAL(8) :: max_unemp_ee
    REAL(8) :: max_unemp_young_ee
    REAL(8) :: max_unemp_prime_ee

    REAL(8) :: unemp_ee_p5,unemp_ee_p10,unemp_ee_p25,unemp_ee_p50,unemp_ee_p75,unemp_ee_p90,unemp_ee_p95

    REAL(8) :: occmob_3m_p33, occmob_3m_p67, occmob_6m_p33, occmob_6m12_p67

    INTEGER :: warningcounter, improv_ind
    INTEGER(4) :: errmsg

    !CHARACTER*12 :: x1
    !INTEGER(4):: i1
    !LOGICAL :: file_exists


    !!OPEN(UNIT=33, file='smd_trace.txt', status='replace', form='formatted')
    !

    ! -----------------------------------------------------------
    !   FOR MATH KERNEL LIBRARY PSEUDO-RANDOM NUMBER GENERATOR
    ! -------------------------------------------------------
      integer n
      integer(kind=4) errcode

      integer brng,method,seed

      TYPE (VSL_STREAM_STATE) :: stream

    !------------------------------------------
    ! WRITE OUTPUT TO FILE
    !------------------------------------------
    !call MPI_INIT(ierr)
    !CALL MPI_COMM_RANK(MPI_COMM_WORLD, threadnumber, ierr)
    threadnumber=mpthread
    i1= threadnumber
    !WRITE(*,*) 'thread reporting', i1
    WRITE(x1,FMT='(I3.3)') i1 ! converting integer to string using a 'internal file'
    filenm='output_'//trim(file_id)//trim(x1)//'.txt'

    INQUIRE(FILE=filenm, EXIST=file_exists)
    IF(file_exists) THEN
        IF((estim_counter/2000)*2000 .NE. estim_counter) THEN
            ! append if not 100 iterations
            OPEN(unit=10,file=filenm,status="old", form="formatted", position='append')
        ELSE
            ! replace file when more than a 100 iterations have been recorded
            OPEN(unit=10,file=filenm,status="replace",form="formatted")
        END IF
    ELSE
        OPEN(unit=10,file=filenm,status="replace",form="formatted")
    END IF


   !-----------
   ! SOME INITIAL INITIALIZATIONS, SETTINGS
   !---------------------
    restricted_search_ind=0
    runnumber=0
    expts=tmax

    tmin_sim2=tmin_sim2basic/12*12


    !--------------------------------
    ! THREADNUMBERS (BOTH FOR MPI AND OPENMP -- VERIFY!)
    !--------------------------------




        !WRITE(*,*) 'MODEL PERIOD ------ WEEKLY'
        !youngcutoff=480                                               ! cutoff for young
        ! AVERAGE AGE OF 20-30yo who have entered, and earlier employment (even with
        ! su tage [w=pweight2] if entry_ind==1 & earlier_empl==1 & tage>=20 & tage<=30 & wave>4

        !    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
        !-------------+-----------------------------------------------------------------
        !        tage | 1.3e+06  1503015.98    25.92234   2.810108         20         30


        !. su tage [w=pweight2] if earlier_empl==1 & tage>=18 & tage<=30 & wave>4
        !(analytic weights assumed)
        !
        !    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
        !-------------+-----------------------------------------------------------------
        !        tage | 1.5e+06  1706244.81     25.3576   3.070447         20         30



        youngcutoff=432         ! average 9 years in the market when young; enter on average at age 21.
        lowcutoff=672                                                 ! lower cutoff for prime
        highcutoff=1632 !max(tmax_sim2, tmax_sim) !2160                     ! higher cutoff for prime 2160/48=45
        gencutoff=max(tmax_sim2, tmax_sim) !2160                      ! highest cutoff for all: 40 years on the labor market
        highcutoff2=gencutoff !highcutoff  !=gencutoff                           ! age cutoff for cm(dur) calculations.
        bt=0.9997_8                                    !bt=0.9992_8
        dd=0.00048_8

IF(threadnumber==1 .AND. verboseswitch_l2==1) THEN
    WRITE(10,*) 'hp filter factor =', hpfilterfactor
    WRITE(10,*) '------------------------------------------'
    IF (instantreall==0) WRITE(10,*) '1) REALLOCATION TAKES ONE PERIOD'
    IF (instantreall==1) WRITE(10,*) '1) CAN APPLY FOR A JOB WITHIN THE SAME PERIOD AS REALLOCATION'
    IF (prodfunc_add==0) WRITE(10,*) '2) MULTIPLICATIVE production function *******'
    IF (prodfunc_add==1) WRITE(10,*) '2) ADDITIVE production function *******'
    IF (islandhet_only==1) WRITE(10,*) '3) island heterogeneity only'
    IF (islandhet_only==0) WRITE(10,*) '3)match, island, agg heterogeneity'
    WRITE(10,*) '4) islands simulated for each person'
    WRITE(10,*) 'ppts: ', ppts, '-- zpts: ', zpts, '-- plambda: ', plambda, '-- zlambda:', zlambda
    !WRITE(10,*) '4) islands simulated first, then workers put on islands'
END IF !(threadnumber==1)



!$ WRITE(10,*) 'inside thread'
!$ WRITE(10,*) '---------------------',  omp_get_thread_num()



 !================================================
  !----------------------------------------
	!  PARAMETERS -  VERSIONS
	!---------------------------------------
!======================================================

   !Set the parameters: NOW INCLUDES SIGMA_Z


    ! for testing!




 IF (standard_theta_to_pars_ind==1) THEN
   delta=thetal(1)
   reallc=thetal(2)
   k=thetal(3)
   b=thetal(4)

   rho_p=thetal(5)
   sigma_p=thetal(6)

   rho_z=thetal(7)
   sigma_z=thetal(8)
   eta= thetal(9)

   zcorrection=thetal(10)

   IF(xpts>=3) THEN
   xvector(1)=1.0_8
   xvector(2)=thetal(11)
   xvector(3)=thetal(12)
   ELSE IF (xpts==1) THEN
   xvector=1.0_8
   END IF

   directed_parameter=1.0_8
   skilldep=0.0_8
   skilldep=thetal(13)

    IF(heterogenous_delta_ind==1) THEN
        deltalowhc=thetal(1)
        deltahighhc=thetal(14)
    ELSE
        deltalowhc=delta
        deltahighhc=delta
    END IF

ELSE IF (test_switch==1) THEN
        delta=0.000169
        reallc=12.060150
        k=3.834998
        b=0.813719
        rho_p=0.989900
        sigma_p=0.002654
        rho_z=0.999126
        sigma_z=0.006337
        eta=0.049000
        zcorrection=0.445169
        xvector(2)=1.045977
        xvector(3)=1.585762
        directed_parameter=1.000000
        skilldep=0.0_8

END IF









    !====================================
    !  SET AUXILIARY VARIABLES, TO BE CHANGED LATER (MOVE TO GLOBAL)
    !=====================================


   IF (miscoding_estimation_ind==1) THEN
       directed_parameter=1.0
       miscoding_estimation=thetal(13)

   END IF
    IF (miscoding_parameter_ind==1) THEN
       miscoding_estimation=miscoding_par+miscoding_par-(miscoding_par*(miscoding_par/10.0_8))  !incorporating that miscoding can occur on both sides of the unemployment spell
                                                                                                !, but that a miscoding that involves two times the exact same occupation is unlikely (hence the division by a factor 10)
   END IF

   sep_cutoff=dd+ (1.0_8-dd)*(1.0_8-delta)         ! used in the simulation, implied cutoff for random number used separation
   sep_cutoff_vec=dd+ (1.0_8-dd)*(1.0_8-delta)
   IF (heterogenous_delta_ind==1 .AND. xpts>1) THEN
   sep_cutoff_vec(1)=dd+ (1.0_8-dd)*(1.0_8-deltalowhc)         ! used in the simulation, implied cutoff for random number used separation
   sep_cutoff_vec(2:xpts)=dd+ (1.0_8-dd)*(1.0_8-deltahighhc)         ! used in the simulation, implied cutoff for random number used separation
   END IF
   skilldep_cutoff=dd+(1.0_8-dd)*(1.0_8-skilldep)

   IF(directed_parameter>1.0_8) directed_parameter=1.0_8
   IF(directed_parameter==1.0_8) restricted_search_ind=0
   IF(directed_parameter<0.0_8) directed_parameter=0.0001_8



!============================================
!  FURTHER DATA FOR SMD
!===========================================




    cm_data=0.532              
    lcmy_lcmp_data=0.16_8      
    ac_p_data=0.753_8           ! autocorrelation of production
    sd_p_data=0.00940_8           ! volatility of HP productivity
    !elmatch_data=0.5000_8          ! elasticity of the matching function


    cmaftercm_cmaftercs_data=0.639/0.37              ! occafterocc/cocc_inflow

    ! returns to tenure
    rtnoccten5yr_data=0.154_8   ! returns to 5yr tenure
    rtnoccten10yr_data=0.232_8       ! returns to 10yr tenure

    ! composition with duration


    logwage_disp_diff_all_data=0.05_8
    logwage_disp_diff_highoccten_data=0.07_8


  !================
  ! SOME DATA MOMENTS USED IN THE CALIBRATION
  !================

   CALL set_auxdata_moments
   IF (nun_calib_no_umed_moment_ind==1) THEN
        CALL set_auxdata_moments_nun
   END IF
!============================
!   PRODUCTIVITY EVOLUTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!===========================

!-----------------------------------------------
!  OCC EXPERIENCE, and ACCUMULATION process
!----------------------------------------------


    xtransmatrix=0.0_8
    IF(xpts==3) THEN
        xtransmatrix(1,1)=0.99615_8            ! stochastic aging, weekly model, 5y, 10y
        xtransmatrix(1,2)=0.00385_8
        xtransmatrix(2,2)=0.99615_8
        xtransmatrix(2,3)=0.00385_8
        xtransmatrix(3,3)=1.0_8
    ELSE IF (xpts==1) THEN
        xtransmatrix(1,1)=1.0_8
    END IF
!----------------------------------------------------
!      AGGREGATE PRODUCTIVITY PROCESS
!----------------------------------------------------


CALL sub_tauchen(ppts, plambda, rho_p, sigma_p, pvector, ptransmatrix,0)
!IF (rouwenhorst_ind==1) CALL Rouwenhorst(rho_p, 0.0_8, sigma_p, ppts, pvector, ptransmatrix)
CALL sub_gamastar(ptransmatrix, pnewpdf)
!WRITE(10,*) 'outside tauchen'
!
!     !$OMP MASTER
     IF(printtransitionmatrix_ind==1) THEN
     INQUIRE (FILE='tauchen.txt', EXIST=exists)
        IF(exists) OPEN(UNIT=20, file="tauchen.txt", status="old", form="formatted", position="append")
        IF(exists == .FALSE.) OPEN(UNIT=20, file="tauchen.txt", status="unknown", form="formatted")
!
               WRITE(20,*) ' '
               WRITE(20,*) ' '
               WRITE(20,*) '============================================================='
               WRITE(20,*) '==, rho_p:', rho_p, '==,sigma:',sigma_p
               WRITE(20,*) '-------------------------------------------------------------'
               WRITE(20,FMT='(2(A10))', ADVANCE='no')  'grid,', 'stat dist,'
!
               DO zcnt=1,SIZE(pvector)-1
                        WRITE(20,FMT='(I3,A6,A1)', ADVANCE='no') zcnt,'trans' , ','
               END DO
                        WRITE(20,FMT='(I3,A6,A1)') SIZE(pvector),'trans', ','
!
              DO zcnt=1,SIZE(pvector)
                    WRITE(20,FMT='(2(F9.4,A1))', ADVANCE='no')  pvector(zcnt), ',' , pnewpdf(zcnt), ','
                  DO xcnt=1,SIZE(pvector)-1
                        WRITE(20,FMT='(F9.4,A1)', ADVANCE='no')  ptransmatrix(zcnt,xcnt), ','
                  END DO
!
                  WRITE(20,FMT='(F9.4,A1)')  ptransmatrix(zcnt,SIZE(pvector)), ','
              END DO
!
              WRITE(20,*) 'long run average aggregate productivity is ', sum(pvector(:)*pnewpdf(:))
       CLOSE(20)
       END IF
       !$OMP END MASTER
!
CALL sub_tauchen(zpts, zlambda, rho_z, sigma_z, zvector, ztransmatrix, 0)       ! 0,1 is precision_ind, creates more precision at the bottom
!IF (rouwenhorst_ind==1) CALL Rouwenhorst(rho_z, 0.0_8, sigma_z, zpts, zvector, ztransmatrix)



CALL sub_gamastar(ztransmatrix, znewpdf(1:zpts))
!WRITE(10,*) 'outside z-tauchen-2'

znewcdf(1)=znewpdf(1)


  DO zcnt=2, zpts
    !WRITE(10,*) zvector(zcnt), znewpdf(zcnt)
    znewcdf(zcnt)=znewcdf(zcnt-1)+znewpdf(zcnt)
  END DO
znewcdf(zpts)=1.0_8

zvector=zvector-zcorrection


!---------------------------------------------------------
!                   NEW Z DISTRIBUTION: drawn from the stationary distribution of z
!---------------------------------------------------------

!CALL sub_gamastar(ztransmatrix, znewpdf)

IF(sum(znewpdf)<0.99 .OR. sum(znewpdf)>1.01) THEN
 WRITE(*,*) 'mistake in stationary z distribution'
znewcdf(1)=znewpdf(1)
!WRITE(*,*) zvector(1), znewpdf(1)
  IF (threadnumber==1) THEN
  DO zcnt=2, zpts
      IF(verboseswitch==1) WRITE(10,*) zvector(zcnt), znewpdf(zcnt)
    znewcdf(zcnt)=znewcdf(zcnt-1)+znewpdf(zcnt)
  END DO
  END IF
  !PAUSE
END IF





!------------------------------------------------
! PRODUCTION MATRIX ASSIGNMENT
!-----------------------------------------------


DO pcnt=1, ppts
        DO xcnt=1, xpts
            DO zcnt=1, zpts
               prod(pcnt, xcnt, zcnt)=pfunction(pcnt, xcnt, zcnt)
            END DO
        END DO
END DO


!-------------------------
! ADD RESTRICTED SEARCH //// DISPLAY PRODUCTIVITY DISTRIBUTIONS
!------------------------

IF (threadnumber==1 .AND. verbose_results_l1==2) THEN
WRITE(10,*) 'zvector(1): ', zvector(1), '-- pdf:', znewpdf(1), 'cdf:', znewcdf(1)
WRITE(10,*) 'zvector(zpts): ', zvector(zpts), '-- pdf:', znewpdf(zpts), 'cdf:', znewcdf(zpts)
END IF

        ! explanation: directed_parameter gives the mass of islands considered, i.e. 0.2, means that
        ! the top 20% of islands is considered

znewpdf_under_restricted_search: IF (restricted_search_ind==1 .AND. directed_parameter<1.00_8) THEN

tempreal=0.0_8
z_restricted_pdf=0.0_8
z_restricted_cdf=0.0_8

IF (znewpdf(zpts)>=directed_parameter) THEN
        WRITE(10,*) 'znewpdf, directed_parameter', znewpdf(zpts), directed_parameter
        z_restricted_pdf(zpts)=1.0_8
        z_restricted_cdf(zpts)=1.0_8
        tempint=zpts
ELSE
    tempreal=0.0_8
    tempcounter1=0
    tempint=1
    restricted_do: DO zcnt=zpts, 1, -1
        IF (tempreal+znewpdf(zcnt)>=directed_parameter ) THEN       !.AND. tempcounter1==0
            z_restricted_pdf(zcnt)=(directed_parameter-tempreal)/(directed_parameter)
            z_restricted_cdf(zcnt)=(directed_parameter-tempreal)/(directed_parameter)
            tempint=zcnt+1
            tempcounter1=1
            EXIT restricted_do
        ELSE !! IF (tempcounter1==0) THEN
            tempreal=tempreal+znewpdf(zcnt)
        END IF
    END DO  restricted_do

    DO zcnt=max(1,tempint), zpts
    z_restricted_pdf(zcnt)=znewpdf(zcnt)/directed_parameter                 !! make sure tempint>0
    z_restricted_cdf(zcnt)=z_restricted_cdf(zcnt-1)+z_restricted_pdf(zcnt)
    END DO
END IF

CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
WRITE(10,*) 'date=', time_string(1)
WRITE(10,*) 'time=', time_string(2)



IF (z_restricted_cdf(zpts)>1.001_8 .OR. z_restricted_cdf(zpts)<0.999_8) THEN
     WRITE(*,*) (z_restricted_cdf(i), i=1,zpts )
     WRITE(*,*) 'ERROR IN THE RESTRICTED SEARCH CUTOFF'
        STOP
END IF
zoldpdf=znewpdf
znewpdf=z_restricted_pdf

zoldcdf=znewcdf
znewcdf=z_restricted_cdf


IF(verboseswitch==1) WRITE(*,*) 'stationary z distribution, zvector, pvector'
DO zcnt=1, zpts
 IF(verboseswitch==1) WRITE(*,*) znewpdf(zcnt), zvector(zcnt), pvector(MIN(zcnt,ppts))
END DO



IF(threadnumber==1) THEN
    IF(verboseswitch==1) WRITE(*,*) 'znewpdf: ', znewpdf
    IF(verboseswitch==1) WRITE(*,*) 'znewcdf: ', znewcdf
END IF
tempreal=znewcdf(zpts)
znewcdf=znewcdf/tempreal
znewpdf=znewpdf/tempreal

IF(verboseswitch==1) WRITE(*,*) 'productivity assigned'

END IF znewpdf_under_restricted_search

tempreal=0.0_8
tempreal2=0.0_8




!----------------------------------------------------------------------------------------
!
!   START THE ITERATION BACKWARDS
!               INCORPORATE CONVERGENCE CRITERION, AND STOP
!
!---------------------------------------------------------------------------------------
!IF (marker_ind==1) WRITE(*,*) 'BACKWARDS'



!--------------------
! ALLOCATING THE ARRAYS
!--------------------


CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
IF(verboseswitch==1) WRITE(*,*) 'date=', time_string(1), 'time=', time_string(2)
time_string_begin=time_string
call cpu_time ( t1 )

  !$OMP CRITICAL
ALLOCATE(poliDW(ppts,xpts, zpts), & !TH(ypts, 0:expts, zpts, tmax),TH_U(ypts, 0:expts, tmax)&
                        PoliR(ppts, xpts, zpts), & !EV(ppts, zpts, 2),REALLOC(ppts), DmaxU(ppts, xpts, zpts)
                        JF(ppts, xpts, zpts) , WAGE(ppts, xpts,zpts))
  !$OMP END CRITICAL


    poliR(:,:,:)=0
    poliDW(:,:,:)=0
    JF(:,:,:)=0.0_8
    !EV(:,:,:)=0.0_8
    DmaxU(:,:,:)=0.0_8
    REALLOC(:)=0.0_8


  !!--------------------------
  !! INITIAL GUESS VE, VU, zr, zs
  !!--------------------------


    VE(:,:,:)=b/(1.0-bt*(1.0-dd))
    VU(:,:,:)=b/(1.0-bt*(1.0-dd))




! put separation cutoffs half way (no need to calculate it more precise, as that routine will be repeated inside ctv_backwards_induct
reservation_zr(1:ppts, 1:xpts)=(zpts/2)+1
reservation_zq(1:ppts, 1:xpts)=(zpts/2)+1


  !--------------------------------
  ! CALL BACKWARDS INDUCTION
  !--------------------------------

CALL ctv_backwards_induct(VE,VU, reservation_zr, reservation_zq, poliDW, poliR, jf, wage, &
                            prod, ztransmatrix, ptransmatrix, xtransmatrix, znewpdf, threadnumber, skilldep, pvector, deltalowhc, deltahighhc)


!---------- some more housekeeping
!  !$OMP CRITICAL
!DEALLOCATE(VE, VU)  ! ev excluded
!  !$OMP END CRITICAL

CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
IF(verboseswitch==1) WRITE(10,*) 'start time simulations=', time_string(2)


    !******************************************************************
    !
    ! II. SIMULATION
    !
    !******************************************************************

    !1. All workers start unemployed, randomly assigned to one island
    !2. Convention: ustate is recorded post-search at t and indexed as t. The state x (promised value)
    !is recorded post-search at t, the promised value choice for next period, i.e. the continuation
    !value into the next period is indexed t+1 (overwritten if a succesfull application yields a better promised value.
    !Dummies and wages are recorded
    !post-search at t and indexed by t (helps also in computing wage moments by status later).
    !3. Whenever w(t)=0, could set w(t)=b IF the income measurement in the data includes UI benefits.

    !PRINT *, "Simulating"

    ! -----------------------------------------------------------
    !   FOR MATH KERNEL LIBRARY PSEUDO-RANDOM NUMBER GENERATOR
    ! -------------------------------------------------------

      brng=VSL_BRNG_SFMT19937
      method=VSL_RNG_METHOD_UNIFORM_STD

      ! initialize it
      seed=1


!     ***** Initialize *****
      errcode=vslnewstream( stream, brng,  seed )
      CALL CheckVslError(errcode)



  !$OMP CRITICAL
ALLOCATE(aggprod_ind(tmax_sim2))
  !$OMP END CRITICAL



 aggprod_ind(:)=0
p_distribution_sim=0.0_8
!-------------------------------------------------------------------------------------
! GENERATE TIME SERIES FOR PRODUCTIVITY AND ISLANDS
!======================================================
!---------------------------------------------------------------------------------------

! AGGREGATE PRODUCTIVITY SERIES MAPS T into P
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


CALL RANDOM_SEED()

! start aggregate quality at 1
aggprod_ind(1)=INT(ppts/2)


!CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
!ISLAND: generate random number, then find quality
!WRITE(10,*) 'simulating aggregate productivity time series', 'time=', time_string(2)


!CALL ranlux(prob,tmax_sim2)
!  !$OMP CRITICAL
!CALL RANDOM_NUMBER(prob)
!  !$OMP END CRITICAL


!     ***** Call RNG *****
errcode=vsrnguniform( method, stream, tmax_sim2, prob, 0.0, 1.0)

DO t=2, tmax_sim2
            tempreal=prob(t)

                    tempint=1
                    tempreal6=0.0_8
                    temp_p_cdf=0.0_8
                    DO p2cnt=1, ppts
                       tempreal6=ptransmatrix(aggprod_ind(t-1), p2cnt)+tempreal6
                       temp_p_cdf(p2cnt)=tempreal6
                    END DO

                    DO p2cnt=1, ppts

                        

                        IF (tempreal>temp_p_cdf(p2cnt)) THEN
                            tempint=tempint+1
                        END IF

                            IF (tempint>ppts) THEN
                                WRITE(*,*) 'Mistake in simulation, attempts to assign a p>ppts'
                                STOP
                            END IF
                    END DO
             aggprod_ind(t)=tempint

            p_distribution_sim(aggprod_ind(t))=p_distribution_sim(aggprod_ind(t))+1.0_8
END DO
p_distribution_sim=p_distribution_sim/sum(p_distribution_sim(:))



!---------------------------------
!  ALLOCATE DATA SERIES
!---------------------------------

counter1=((tmax_sim2-12-MAX((tmin_sim2/12)*12,5))/12)+5+20

  !$OMP CRITICAL
ALLOCATE(unempdur_estatus((tmax_sim2/4)+4), data_u_qtr(counter1),data_uall_qtr(counter1), data_e_qtr(counter1), &
    data_uadj18m_qtr(counter1), data_uraw_qtr(counter1), &
    data_u_young_qtr(counter1),data_u_prime_qtr(counter1), data_uadj18m_young_qtr(counter1),&
    data_uadj18m_prime_qtr(counter1), data_e_young_qtr(counter1),data_e_prime_qtr(counter1), &
data_u_qtr_p33(counter1), data_u_qtr_p50(counter1), data_u_qtr_p67(counter1), &
data_ucompldur_occ_qtr(counter1), data_ucompldur_young_occ_qtr(counter1), data_ucompldur_prime_occ_qtr(counter1), &
data_ucompldur_nocc_qtr(counter1), data_ucompldur_young_nocc_qtr(counter1), data_ucompldur_prime_nocc_qtr(counter1), &
data_totduration_cocc_qtr(counter1,0:18), data_totduration_nocc_qtr(counter1,0:18), &
data_totduration_cocc_young_qtr(counter1,0:18), data_totduration_nocc_young_qtr(counter1,0:18),&
data_totduration_cocc_prime_qtr(counter1,0:18), data_totduration_nocc_prime_qtr(counter1,0:18),&
data_compduration_cocc_qtr(counter1,0:18), data_compduration_nocc_qtr(counter1,0:18), &
data_compduration_cocc_young_qtr(counter1,0:18), data_compduration_nocc_young_qtr(counter1,0:18),&
data_compduration_cocc_prime_qtr(counter1,0:18), data_compduration_nocc_prime_qtr(counter1,0:18),&
data_totduration_cr_cocc_qtr(counter1,0:18), data_totduration_cr_nocc_qtr(counter1,0:18),&
data_totduration_cr_cocc_young_qtr(counter1,0:18), data_totduration_cr_nocc_young_qtr(counter1,0:18),&
data_totduration_cr_cocc_prime_qtr(counter1,0:18), data_totduration_cr_nocc_prime_qtr(counter1,0:18),&
data_v_qtr(counter1),data_theta_qtr(counter1), &
data_jf_qtr(counter1), data_jf_young_qtr(counter1), data_jf_prime_qtr(counter1), &
data_prod_qtr(counter1), data_wage_qtr(counter1), data_sep_qtr(counter1), data_sep_y_qtr(counter1), &
data_sep_p_qtr(counter1), &
data_cocc_inflow_qtr(counter1), data_cocc_young_inflow_qtr(counter1), data_cocc_prime_inflow_qtr(counter1), data_nocc_inflow_qtr(counter1), &
data_nocc_young_inflow_qtr(counter1), data_nocc_prime_inflow_qtr(counter1), &
data_cocc_outflow_qtr(counter1), data_cocc_young_outflow_qtr(counter1), data_cocc_prime_outflow_qtr(counter1), data_nocc_outflow_qtr(counter1), &
data_nocc_young_outflow_qtr(counter1), data_nocc_prime_outflow_qtr(counter1), &
data_cocc_qtr(counter1), data_cocc_young_qtr(counter1), data_cocc_prime_qtr(counter1), data_nocc_qtr(counter1), &
data_nocc_young_qtr(counter1), data_nocc_prime_qtr(counter1), data_pocc_qtr(counter1), data_pnocc_qtr(counter1), &
data_pocc_young_qtr(counter1), data_pnocc_young_qtr(counter1), data_pocc_prime_qtr(counter1), data_pnocc_prime_qtr(counter1), &
data_young_qtr(counter1), data_prime_qtr(counter1), data_ult5wk_qtr(counter1), data_u5lt15wk_qtr(counter1), &
data_u15lt27wk_qtr(counter1), data_ugt27wk_qtr(counter1), data_l_ult5wk_qtr(counter1), data_l_u5lt15wk_qtr(counter1), &
data_l_u15lt27wk_qtr(counter1), data_l_ugt27wk_qtr(counter1), data_ult3m_qtr(counter1), data_ult5m_qtr(counter1), &
data_ult9m_qtr(counter1), data_ult13m_qtr(counter1), data_ugt13m_qtr(counter1), &
data_l_ult3m_qtr(counter1), data_l_ult5m_qtr(counter1), &
data_l_ult9m_qtr(counter1), data_l_ult13m_qtr(counter1), data_l_ugt13m_qtr(counter1), &
data_ult3m_yng_qtr(counter1), data_ult5m_yng_qtr(counter1), &
data_ult9m_yng_qtr(counter1), data_ult13m_yng_qtr(counter1), data_ugt13m_yng_qtr(counter1), &
data_l_ult3m_yng_qtr(counter1), data_l_ult5m_yng_qtr(counter1), &
data_l_ult9m_yng_qtr(counter1), data_l_ult13m_yng_qtr(counter1), data_l_ugt13m_yng_qtr(counter1), &
data_ult3m_prm_qtr(counter1), data_ult5m_prm_qtr(counter1), &
data_ult9m_prm_qtr(counter1), data_ult13m_prm_qtr(counter1), data_ugt13m_prm_qtr(counter1), &
data_l_ult3m_prm_qtr(counter1), data_l_ult5m_prm_qtr(counter1), &
data_l_ult9m_prm_qtr(counter1), data_l_ult13m_prm_qtr(counter1), data_l_ugt13m_prm_qtr(counter1), &
data_scratch_qtr(counter1)) !data_temp_qtr(counter1), data_temp_qtr2(counter1))
  !$OMP END CRITICAL

!CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
!WRITE(10,*) 'allocated data series:', time_string(2)

tot_costofhires=0.0
tot_hires=0.0_8

unempdur_estatus=-2

 data_young_qtr=0.0_8
 data_prime_qtr=0.0_8

 data_u_qtr=0.0_8
 data_uall_qtr=0.0_8
 data_uraw_qtr=0.0_8
 data_e_qtr=0.0_8
 data_uadj18m_qtr=0.0_8
 data_u_young_qtr=0.0_8
 data_u_prime_qtr=0.0_8
 data_uadj18m_young_qtr=0.0_8
 data_uadj18m_prime_qtr=0.0_8

 data_e_young_qtr=0.0_8
 data_e_prime_qtr=0.0_8

 data_ucompldur_occ_qtr=0.0_8
 data_ucompldur_young_occ_qtr=0.0_8
 data_ucompldur_prime_occ_qtr=0.0_8

 data_ucompldur_nocc_qtr=0.0_8
 data_ucompldur_young_nocc_qtr=0.0_8
 data_ucompldur_prime_nocc_qtr=0.0_8

 data_u_qtr_p33=0.0_8
 data_u_qtr_p50=0.0_8
 data_u_qtr_p67=0.0_8

 data_compduration_cocc=0.0_8
 data_compduration_nocc=0.0_8
 data_compduration_cocc_young=0.0_8
 data_compduration_nocc_young=0.0_8
 data_compduration_cocc_prime=0.0_8
 data_compduration_nocc_prime=0.0_8


 data_totduration_cocc_qtr=0.0_8
 data_totduration_nocc_qtr=0.0_8
 data_totduration_cocc_young_qtr=0.0_8
 data_totduration_nocc_young_qtr=0.0_8
 data_totduration_cocc_prime_qtr=0.0_8
 data_totduration_nocc_prime_qtr=0.0_8
 data_compduration_cocc_qtr=0.0_8
 data_compduration_nocc_qtr=0.0_8
 data_compduration_cocc_young_qtr=0.0_8
 data_compduration_nocc_young_qtr=0.0_8
 data_compduration_cocc_prime_qtr=0.0_8
 data_compduration_nocc_prime_qtr=0.0_8


 data_totduration_cr_cocc_qtr=0.0_8
 data_totduration_cr_nocc_qtr=0.0_8
 data_totduration_cr_cocc_young_qtr=0.0_8
 data_totduration_cr_nocc_young_qtr=0.0_8
 data_totduration_cr_cocc_prime_qtr=0.0_8
 data_totduration_cr_nocc_prime_qtr=0.0_8

 jf_ave2=0.0_8
 jf_ave2_counter=0

 data_v_qtr=0.0_8
 data_theta_qtr=0.0_8
 data_jf_qtr=0.0_8
 data_jf_young_qtr=0.0_8
 data_jf_prime_qtr=0.0_8

 data_prod_qtr=0.0_8
 data_wage_qtr=0.0_8
 data_sep_qtr=0.0_8
 data_sep_y_qtr=0.0_8
 data_sep_p_qtr=0.0_8

 data_cocc_inflow_qtr=0.0_8
 data_cocc_young_inflow_qtr=0.0_8
 data_cocc_prime_inflow_qtr=0.0_8
 data_nocc_inflow_qtr=0.0_8
 data_nocc_young_inflow_qtr=0.0_8
 data_nocc_prime_inflow_qtr=0.0_8

 data_cocc_outflow_qtr=0.0_8
 data_cocc_young_outflow_qtr=0.0_8
 data_cocc_prime_outflow_qtr=0.0_8
 data_nocc_outflow_qtr=0.0_8
 data_nocc_young_outflow_qtr=0.0_8
 data_nocc_prime_outflow_qtr=0.0_8


 data_cocc_qtr=0.0_8
 data_cocc_young_qtr=0.0_8
 data_cocc_prime_qtr=0.0_8
 data_nocc_qtr=0.0_8
 data_nocc_young_qtr=0.0_8
 data_nocc_prime_qtr=0.0_8

 data_pocc_qtr=0.0_8
 data_pnocc_qtr=0.0_8
 data_pocc_young_qtr=0.0_8
 data_pnocc_young_qtr=0.0_8
 data_pocc_prime_qtr=0.0_8
 data_pnocc_prime_qtr=0.0_8

data_ult5wk_qtr=0.0_8
data_u5lt15wk_qtr=0.0_8
data_u15lt27wk_qtr=0.0_8
data_ugt27wk_qtr=0.0_8
data_l_ult5wk_qtr=0.0_8
data_l_u5lt15wk_qtr=0.0_8
data_l_u15lt27wk_qtr=0.0_8
data_l_ugt27wk_qtr=0.0_8

data_ult3m_qtr=0.0_8
data_ult5m_qtr=0.0_8
data_ult9m_qtr=0.0_8
data_ult13m_qtr=0.0_8
data_ugt13m_qtr=0.0_8
data_l_ult3m_qtr=0.0_8
data_l_ult5m_qtr=0.0_8
data_l_ult9m_qtr=0.0_8
data_l_ult13m_qtr=0.0_8
data_l_ugt13m_qtr=0.0_8


data_ult3m_yng_qtr=0.0_8
data_ult5m_yng_qtr=0.0_8
data_ult9m_yng_qtr=0.0_8
data_ult13m_yng_qtr=0.0_8
data_ugt13m_yng_qtr=0.0_8
data_l_ult3m_yng_qtr=0.0_8
data_l_ult5m_yng_qtr=0.0_8
data_l_ult9m_yng_qtr=0.0_8
data_l_ult13m_yng_qtr=0.0_8
data_l_ugt13m_yng_qtr=0.0_8

data_ult3m_prm_qtr=0.0_8
data_ult5m_prm_qtr=0.0_8
data_ult9m_prm_qtr=0.0_8
data_ult13m_prm_qtr=0.0_8
data_ugt13m_prm_qtr=0.0_8
data_l_ult3m_prm_qtr=0.0_8
data_l_ult5m_prm_qtr=0.0_8
data_l_ult9m_prm_qtr=0.0_8
data_l_ult13m_prm_qtr=0.0_8
data_l_ugt13m_prm_qtr=0.0_8

! data_temp_qtr=0.0_8

 counter_u_window=0
 counter_u_window_y=0
 counter_u_window_p=0
 counter_u_window_all=0
 counter_u_window_all_y=0
 counter_u_window_all_p=0
 counter_uocc_window=0
 counter_uocc_window_all=0
 counter_unocc_window=0
 counter_unocc_window_all=0


 sep_ave_1m=0.0_8
 sep_ave_2m=0.0_8
 sep_ave_3m=0.0_8
 sep_ave_4m=0.0_8
 sep_ave_5m=0.0_8
 sep_ave_6m=0.0_8
 sep_ave_7m=0.0_8
 sep_ave_8m=0.0_8
 sep_ave_9m=0.0_8
 sep_ave_10m=0.0_8
 sep_ave_11m=0.0_8
 sep_ave_12m=0.0_8
 sep_ave_m_counter=0

 sep_ave_1m_posthire=0.0_8
 sep_ave_2m_posthire=0.0_8
 sep_ave_3m_posthire=0.0_8
 sep_ave_4m_posthire=0.0_8
 sep_ave_5m_posthire=0.0_8
 sep_ave_6m_posthire=0.0_8
 sep_ave_7m_posthire=0.0_8
 sep_ave_8m_posthire=0.0_8
 sep_ave_9m_posthire=0.0_8
 sep_ave_10m_posthire=0.0_8
 sep_ave_11m_posthire=0.0_8
 sep_ave_12m_posthire=0.0_8
 sep_ave_m_posthire_counter=0

 earlier_e_counter=0
 young_earlier_e_counter=0
 prime_earlier_e_counter=0
 unemp_earlier_e_ave=0.0_8
 u_earlier_e_young_ave=0.0_8
 u_earlier_e_prime_ave=0.0_8

 warningcounter=0

 threadno_if1: IF (threadnumber==1) THEN
 IF(workerhistory_sim==1) THEN
     OPEN(unit=14, file='workerhistory.txt', status='replace', form='formatted')
 END IF
 END IF threadno_if1


!---------
! POPULATION PYRAMID
!------------




! SIMPLE each period, for max_gen periods, nsim_gen people are added
!       more complex things possible

!!! FOR NOW, simple calculation of number of agents
number_agents(:)=nsim_gen















!-------- INITIALIZE

ult5wk=0.0_8
u5lt15wk=0.0_8
u15lt27wk=0.0_8
ugt27wk=0.0_8


duration2=0.0_8
duration2_young=0.0_8
duration2_prime=0.0_8


duration2=0.0_8
duration2_young=0.0_8
duration2_prime=0.0_8

duration2_move=0.0_8
duration2_stay=0.0_8
duration2_move_young=0.0_8
duration2_stay_young=0.0_8
duration2_move_prime=0.0_8
duration2_stay_prime=0.0_8

empduration_occ=0.0_8
empduration_nocc=0.0_8
empduration_occ_Y=0.0_8
empduration_nocc_y=0.0_8
empduration_occ_p=0.0_8
empduration_nocc_p=0.0_8
empduration_occ_shuspell=0.0_8
empduration_nocc_shuspell=0.0_8
empduration_occ_luspell=0.0_8
empduration_nocc_luspell=0.0_8


sepocc_ave=0.0_8
sepnocc_ave=0.0_8
sepocc_y_ave=0.0_8
sepnocc_y_ave=0.0_8
sepocc_p_ave=0.0_8
sepnocc_p_ave=0.0_8
sepocc_ave_counter=0.0_8
sepnocc_ave_counter=0.0_8
sepocc_y_ave_counter=0.0_8
sepnocc_y_ave_counter=0.0_8
sepocc_p_ave_counter=0.0_8
sepnocc_p_ave_counter=0.0_8

sepocc_afternocc_2_5yr=0.0_8
sepocc_afternocc_2_5yr_2m=0.0_8
sepocc_afternocc_3yr=0.0_8
sep_afternocc_2_5yr=0.0_8
sep_afternocc_2_5yr_2m=0.0_8
sepnocc_afternocc_2_5yr=0.0_8
sepnocc_afternocc_2_5yr_2m=0.0_8

sepocc_afternocc_2_5yr_counter=0
sepocc_afternocc_3yr_counter=0
sepnocc_afternocc_2_5yr_counter=0
sep_afternocc_2_5yr_counter=0


returnstenure5y=0.0_8
returnstenure10y=0.0_8

returnstenure5ycounter=0
returnstenure10ycounter=0

tot_occafterocc=0.0_8
tot_occafternocc=0.0_8
tot_noccafterocc=0.0_8
tot_noccafternocc=0.0_8
tot_occafterocc_y=0.0_8
tot_occafternocc_y=0.0_8
tot_noccafterocc_y=0.0_8
tot_noccafternocc_y=0.0_8
tot_occafterocc_p=0.0_8
tot_occafternocc_p=0.0_8
tot_noccafterocc_p=0.0_8
tot_noccafternocc_p=0.0_8


tot_corr_occafterocc=0.0_8
tot_corr_occafternocc=0.0_8
tot_corr_noccafterocc=0.0_8
tot_corr_noccafternocc=0.0_8
tot_corr_occafterocc_y=0.0_8
tot_corr_occafternocc_y=0.0_8
tot_corr_noccafterocc_y=0.0_8
tot_corr_noccafternocc_y=0.0_8
tot_corr_occafterocc_p=0.0_8
tot_corr_occafternocc_p=0.0_8
tot_corr_noccafterocc_p=0.0_8
tot_corr_noccafternocc_p=0.0_8


tot_occafterocc_vector=0.0_8
tot_occafternocc_vector=0.0_8
tot_noccafterocc_vector=0.0_8
tot_noccafternocc_vector=0.0_8

tot_occafterocc_vector_young=0.0_8
tot_occafternocc_vector_young=0.0_8
tot_noccafterocc_vector_young=0.0_8
tot_noccafternocc_vector_young=0.0_8

tot_occafterocc_vector_prime=0.0_8
tot_occafternocc_vector_prime=0.0_8
tot_noccafterocc_vector_prime=0.0_8
tot_noccafternocc_vector_prime=0.0_8


stock_occafterocc=0.0_8
stock_occafternocc=0.0_8
stock_noccafterocc=0.0_8
stock_noccafternocc=0.0_8
stock_occafterocc_y=0.0_8
stock_occafternocc_y=0.0_8
stock_noccafterocc_y=0.0_8
stock_noccafternocc_y=0.0_8
stock_occafterocc_p=0.0_8
stock_occafternocc_p=0.0_8
stock_noccafterocc_p=0.0_8
stock_noccafternocc_p=0.0_8

poccafterocc=0.0_8
poccafternocc=0.0_8
pnoccafterocc=0.0_8
pnoccafternocc=0.0_8

poccafterocc_y=0.0_8
poccafternocc_y=0.0_8
pnoccafterocc_y=0.0_8
pnoccafternocc_y=0.0_8

poccafterocc_p=0.0_8
poccafternocc_p=0.0_8
pnoccafterocc_p=0.0_8
pnoccafternocc_p=0.0_8


poccafterocc_uspell=0.0_8
poccafternocc_uspell=0.0_8
pnoccafterocc_uspell=0.0_8
pnoccafternocc_uspell=0.0_8

poccafterocc_y_uspell=0.0_8
poccafternocc_y_uspell=0.0_8
pnoccafterocc_y_uspell=0.0_8
pnoccafternocc_y_uspell=0.0_8

poccafterocc_p_uspell=0.0_8
poccafternocc_p_uspell=0.0_8
pnoccafterocc_p_uspell=0.0_8
pnoccafternocc_p_uspell=0.0_8


stock_occafterocc_uspell=0.0_8
stock_occafternocc_uspell=0.0_8
stock_noccafterocc_uspell=0.0_8
stock_noccafternocc_uspell=0.0_8
stock_occafterocc_y_uspell=0.0_8
stock_occafternocc_y_uspell=0.0_8
stock_noccafterocc_y_uspell=0.0_8
stock_noccafternocc_y_uspell=0.0_8
stock_occafterocc_p_uspell=0.0_8
stock_occafternocc_p_uspell=0.0_8
stock_noccafterocc_p_uspell=0.0_8
stock_noccafternocc_p_uspell=0.0_8


occafterocc_uspell=0.0_8
occafternocc_uspell=0.0_8
noccafterocc_uspell=0.0_8
noccafternocc_uspell=0.0_8




! REGULAR duration matrices
tot_durationmatrix=0.0_8
tot_durationmatrix2=0.0_8
tot_haz_durationmatrix2=0.0_8
tot_durationmatrix_cocc=0.0_8
tot_durationmatrix_nocc=0.0_8

tot_durationmatrix_young=0.0_8
tot_durationmatrix_cocc_young=0.0_8
tot_durationmatrix_nocc_young=0.0_8

tot_durationmatrix_prime=0.0_8
tot_durationmatrix_cocc_prime=0.0_8
tot_durationmatrix_nocc_prime=0.0_8


tot_durationmatrix2_cocc=0.0
tot_durationmatrix2_cocc_young=0.0
tot_durationmatrix2_nocc_young=0.0
tot_durationmatrix2_nocc=0.0
tot_durationmatrix2_cocc_prime=0.0
tot_durationmatrix2_nocc_prime=0.0


tot_durationmatrix_cr=0.0_8
tot_durationmatrix_cr_cocc=0.0_8
tot_durationmatrix_cr_nocc=0.0_8

tot_durationmatrix_cr_young=0.0_8
tot_durationmatrix_cr_cocc_young=0.0_8
tot_durationmatrix_cr_nocc_young=0.0_8

tot_durationmatrix_cr_prime=0.0_8
tot_durationmatrix_cr_cocc_prime=0.0_8
tot_durationmatrix_cr_nocc_prime=0.0_8

tot_durationmatrix_cocc_p33=0.0_8
tot_durationmatrix_nocc_p33=0.0_8
tot_durationmatrix_cocc_p50=0.0_8
tot_durationmatrix_nocc_p50=0.0_8
tot_durationmatrix_cocc_p67=0.0_8
tot_durationmatrix_nocc_p67=0.0_8


tot_durationmatrix_cocc_young_p33=0.0_8
tot_durationmatrix_nocc_young_p33=0.0_8
tot_durationmatrix_cocc_young_p50=0.0_8
tot_durationmatrix_nocc_young_p50=0.0_8
tot_durationmatrix_cocc_young_p67=0.0_8
tot_durationmatrix_nocc_young_p67=0.0_8


tot_durationmatrix_cocc_prime_p33=0.0_8
tot_durationmatrix_nocc_prime_p33=0.0_8
tot_durationmatrix_cocc_prime_p50=0.0_8
tot_durationmatrix_nocc_prime_p50=0.0_8
tot_durationmatrix_cocc_prime_p67=0.0_8
tot_durationmatrix_nocc_prime_p67=0.0_8


tot_durationmatrix_cr_cocc_p33=0.0_8
tot_durationmatrix_cr_nocc_p33=0.0_8
tot_durationmatrix_cr_cocc_p50=0.0_8
tot_durationmatrix_cr_nocc_p50=0.0_8
tot_durationmatrix_cr_cocc_p67=0.0_8
tot_durationmatrix_cr_nocc_p67=0.0_8


tot_durationmatrix_cr_cocc_young_p33=0.0_8
tot_durationmatrix_cr_nocc_young_p33=0.0_8
tot_durationmatrix_cr_cocc_young_p50=0.0_8
tot_durationmatrix_cr_nocc_young_p50=0.0_8
tot_durationmatrix_cr_cocc_young_p67=0.0_8
tot_durationmatrix_cr_nocc_young_p67=0.0_8


tot_durationmatrix_cr_cocc_prime_p33=0.0_8
tot_durationmatrix_cr_nocc_prime_p33=0.0_8
tot_durationmatrix_cr_cocc_prime_p50=0.0_8
tot_durationmatrix_cr_nocc_prime_p50=0.0_8
tot_durationmatrix_cr_cocc_prime_p67=0.0_8
tot_durationmatrix_cr_nocc_prime_p67=0.0_8


! U CORRELATIONS
u_spell_before_sq=0.0_8
u_spell_before_am_sq=0.0_8
u_spell_before_as_sq=0.0_8
u_spell_before_mas_sq=0.0_8
u_spell_before_mam_sq=0.0_8
u_spell_before_sam_sq=0.0_8
u_spell_before_sas_sq=0.0_8

u_spell_now_sq=0.0_8
u_spell_now_am_sq=0.0_8
u_spell_now_as_sq=0.0_8
u_spell_now_mas_sq=0.0_8
u_spell_now_mam_sq=0.0_8
u_spell_now_sam_sq=0.0_8
u_spell_now_sas_sq=0.0_8

u_spell_before_ave=0.0_8
u_spell_before_am_ave=0.0_8
u_spell_before_as_ave=0.0_8
u_spell_before_mas_ave=0.0_8
u_spell_before_mam_ave=0.0_8
u_spell_before_sam_ave=0.0_8
u_spell_before_sas_ave=0.0_8

u_spell_now_ave=0.0_8
u_spell_now_am_ave=0.0_8
u_spell_now_as_ave=0.0_8
u_spell_now_mas_ave=0.0_8
u_spell_now_mam_ave=0.0_8
u_spell_now_sam_ave=0.0_8
u_spell_now_sas_ave=0.0_8


u_spell_crossterm=0.0_8
u_spell_crossterm_am=0.0_8
u_spell_crossterm_as=0.0_8
u_spell_crossterm_mas=0.0_8
u_spell_crossterm_mam=0.0_8
u_spell_crossterm_sam=0.0_8
u_spell_crossterm_sas=0.0_8

u_spell_young_before_sq=0.0_8
u_spell_young_before_am_sq=0.0_8
u_spell_young_before_as_sq=0.0_8
u_spell_young_before_mas_sq=0.0_8
u_spell_young_before_mam_sq=0.0_8
u_spell_young_before_sam_sq=0.0_8
u_spell_young_before_sas_sq=0.0_8

u_spell_young_now_sq=0.0_8
u_spell_young_now_am_sq=0.0_8
u_spell_young_now_as_sq=0.0_8
u_spell_young_now_mas_sq=0.0_8
u_spell_young_now_mam_sq=0.0_8
u_spell_young_now_sam_sq=0.0_8
u_spell_young_now_sas_sq=0.0_8

u_spell_young_before_ave=0.0_8
u_spell_young_before_am_ave=0.0_8
u_spell_young_before_as_ave=0.0_8
u_spell_young_before_mas_ave=0.0_8
u_spell_young_before_mam_ave=0.0_8
u_spell_young_before_sam_ave=0.0_8
u_spell_young_before_sas_ave=0.0_8

u_spell_young_now_ave=0.0_8
u_spell_young_now_am_ave=0.0_8
u_spell_young_now_as_ave=0.0_8
u_spell_young_now_mas_ave=0.0_8
u_spell_young_now_mam_ave=0.0_8
u_spell_young_now_sam_ave=0.0_8
u_spell_young_now_sas_ave=0.0_8


u_spell_young_crossterm=0.0_8
u_spell_young_crossterm_am=0.0_8
u_spell_young_crossterm_as=0.0_8
u_spell_young_crossterm_mas=0.0_8
u_spell_young_crossterm_mam=0.0_8
u_spell_young_crossterm_sam=0.0_8
u_spell_young_crossterm_sas=0.0_8

u_spell_prime_before_sq=0.0_8
u_spell_prime_before_am_sq=0.0_8
u_spell_prime_before_as_sq=0.0_8
u_spell_prime_before_mas_sq=0.0_8
u_spell_prime_before_mam_sq=0.0_8
u_spell_prime_before_sam_sq=0.0_8
u_spell_prime_before_sas_sq=0.0_8

u_spell_prime_now_sq=0.0_8
u_spell_prime_now_am_sq=0.0_8
u_spell_prime_now_as_sq=0.0_8
u_spell_prime_now_mas_sq=0.0_8
u_spell_prime_now_mam_sq=0.0_8
u_spell_prime_now_sam_sq=0.0_8
u_spell_prime_now_sas_sq=0.0_8

u_spell_prime_before_ave=0.0_8
u_spell_prime_before_am_ave=0.0_8
u_spell_prime_before_as_ave=0.0_8
u_spell_prime_before_mas_ave=0.0_8
u_spell_prime_before_mam_ave=0.0_8
u_spell_prime_before_sam_ave=0.0_8
u_spell_prime_before_sas_ave=0.0_8

u_spell_prime_now_ave=0.0_8
u_spell_prime_now_am_ave=0.0_8
u_spell_prime_now_as_ave=0.0_8
u_spell_prime_now_mas_ave=0.0_8
u_spell_prime_now_mam_ave=0.0_8
u_spell_prime_now_sam_ave=0.0_8
u_spell_prime_now_sas_ave=0.0_8



u_spell_prime_crossterm=0.0_8
u_spell_prime_crossterm_am=0.0_8
u_spell_prime_crossterm_as=0.0_8
u_spell_prime_crossterm_mas=0.0_8
u_spell_prime_crossterm_mam=0.0_8
u_spell_prime_crossterm_sam=0.0_8
u_spell_prime_crossterm_sas=0.0_8



! distribution of (employed and )unemployed with aggregate states
up_distribution_sim=0.0_8
uallp_distribution_sim=0.0_8
eallp_distribution_sim=0.0_8
ep_distribution_sim=0.0_8
up_young_distribution_sim=0.0_8
up_prime_distribution_sim=0.0_8
    ! distribution of (employed and unemployed over islands with different aggregate states
upz_distribution_sim=0.0_8
epz_distribution_sim=0.0_8
upz_young_distribution_sim=0.0_8
epz_young_distribution_sim=0.0_8
upz_prime_distribution_sim=0.0_8
epz_prime_distribution_sim=0.0_8
! distribution of rest/search/reall unemployed with aggregate states
up_rest_distribution_sim=0.0_8
up_young_rest_distribution_sim=0.0_8
up_prime_rest_distribution_sim=0.0_8
up_search_distribution_sim=0.0_8
up_young_search_distribution_sim=0.0_8
up_prime_search_distribution_sim=0.0_8
up_reall_distribution_sim=0.0_8
up_young_reall_distribution_sim=0.0_8
up_prime_reall_distribution_sim=0.0_8

wagepz_distribution_sim=0.0_8
wagepz_young_distribution_sim=0.0_8
wagepz_prime_distribution_sim=0.0_8

epz_x_distribution_sim=0.0_8
epz_x_young_distribution_sim=0.0_8
epz_x_prime_distribution_sim=0.0_8


upz_x_distribution_sim=0.0_8
upz_x_young_distribution_sim=0.0_8
upz_x_prime_distribution_sim=0.0_8

epz_1yb4_x_distribution_sim=0.0_8
sep_epz_1yb4_x_distribution_sim=0.0_8

!! LOG WAGE DISPERSION

logwage_disp_all=0.0_8
logwage_ave_all=0.0_8
logwage_disp_highoccten=0.0_8
logwage_ave_highoccten=0.0_8
logwage_disp_uhires=0.0_8
logwage_ave_uhires=0.0_8

logwage_all_counter=0
logwage_highoccten_counter=0
logwage_uhires_counter=0



!    !---- subsequent reallocations as a function of previous unemployment spell
!    !   ----  in the data we only see this for those people who have multiple unemployment spells within 4 years
!    !   ----  if exogenous breakups are very rare, and a large proportion of measured breakups are endogenous, we have to reproduce these conditions in the program!!!
!    !   ----  this means adding a condition that previous unemployment and employment spell, plus current unemployment spell fall with blocks of 180 weeks.
!    !   ----  this condition is marked by <-----***** on the RHS margin below



!OPEN(UNIT=54, file='random_test.txt', form='formatted', status='replace')

gen_do: DO counter2=1, max_gen              ! MAX number of simultaneous generations
    !IF (((10*counter1)/max_gen)*max_gen-(10*counter1)==0) WRITE(*,*) 'simulation generation no', counter1, 'out of ', max_gen


! assign initial islands, based on znewcdf
initial_island(:)=zpts

tempint=1
DO i=1, nsim_gen
    IF(znewcdf(tempint)<REAL(i)/REAL(nsim_gen)) THEN
        tempint=tempint+1
    ELSE
        initial_island(i)=tempint
    END IF
END DO




nsim_do: DO i=1, nsim_gen


!IF((i/5000)*5000-i==0) WRITE(10,*) 'person i simulation, i=', i, 'on thread ', threadnumber



! initialization of indicators, time variables
e_sim=-8
island_sim=-8
hcten_sim=-8
hcind_sim=-8                    ! initialize -8, newborn -1,
duration_sim=-8
e_duration_sim=-8






! INITIALIZATION t=1
e_sim(counter2)=0
hcten_sim(counter2)=0
hcind_sim(counter2)=-2           !initialize without a sector
duration_sim(counter2)=0
wage_sim(counter2)=0.0_8


!----> ASSIGN NEW ISLAND, znewpdf

!island_sim(counter2)=MAX( INT( ((REAL(i-1.0_8)*REAL(maxislands))/REAL(nsim_gen))    +1.0_8)   ,1)
!            ! assigns islands proportionally with one correction, so first island=1, and last island=zpts
island_sim(counter2)=initial_island(counter2)


!--------------------------------
!   time series for each person
!--------------------------------
!  !$OMP CRITICAL
!CALL RANDOM_NUMBER(jprobs)       !For probability of success in matching when unemployed, and human capital shock when employed
!CALL RANDOM_NUMBER(sepprobs)     !For probability of exogenous separation
!CALL RANDOM_NUMBER(zprobs)       !For realization of idiosyncratic shock
!  !$OMP END CRITICAL

!     ***** Call RNG *****
errcode=vsrnguniform( method, stream, tmax_sim2, jprobs, 0.0, 1.0 )
errcode=vsrnguniform( method, stream, tmax_sim2, sepprobs, 0.0, 1.0 )
errcode=vsrnguniform( method, stream, tmax_sim2, zprobs, 0.0, 1.0 )


!IF((i/10000)*10000-i==0) WRITE(*,*) 'sim no=', i, ', random vectors, 1st elements', workrandom1(1), workrandom2(1), workrandom1(2), workrandom2(2)
IF(verbosesimulation==1) THEN
IF((((i/1000)*1000-i==0 .AND. i<=10000) .OR. ((i/10000)*10000-i==0 .AND. i>10000) .AND. verbosesimulation==1)) THEN
    WRITE(10,FMT='(A10,I2, A10,I6, A20)', ADVANCE="no") 'thread no:', threadnumber, ' sim no=', i, ', random vectors, 1st elements'
    WRITE(10,FMT='(12(F4.3,A1))', ADVANCE='no') jprobs(1),',', jprobs(2),',', jprobs(3),',', jprobs(4),'-', &
                                                     sepprobs(1),',', sepprobs(2),',', sepprobs(3), ',' ,sepprobs(4),'-', &
                                                     zprobs(1),',', zprobs(2),',', zprobs(3), ',' , zprobs(4),'-'
    CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
    WRITE(10,*) 'time=', time_string(2)
END IF
END IF

monthind=0
indiv_timeseries_do: DO t=counter2+1, tmax_sim2



IF ((t/4)*4-t==0 .AND. t>3000) monthind=1  ! month observation


!-----------------------------------------------------
! STAGE 1: DEATH SHOCK, HUMAN CAPITAL ACCUMULATION
!-----------------------------------------------------




survival_if: &
IF(sepprobs(t)<dd .OR. age_sim(t)>(gencutoff+5*48) .OR. (  ( i> ( (REAL(t)-1.0_8)/REAL(tmin_sim2basic) )*REAL(nsim_gen)) .AND.  ( i<=( (REAL(t))/REAL(tmin_sim2basic) )*REAL(nsim_gen)   ))) THEN                ! DEATH AND TAXES -- COULD REPLACED BY NEWBORN. REPLACE SIXTY-YEAR OLDS WITH NEWBORNS
                                                                            ! note that in the lead-in period, people are replaced evenly over time to get the demographics right
                                                                            ! need the lead-in period to cover at least the average working life
e_sim(t)=0
age_sim(t)=0
hcten_sim(t)=0
hcind_sim(t)=-1      ! born with -2, reborn with -1, so not recorded as a reallocation when getting first job on the same island, reallocation 0
                     ! lowest hc level: 1, med hc level 2, hi hc level 3
duration_sim(t)=0
wage_sim(t)=0.0_8


! draw new island
        !CALL RANDOM_NUMBER(tempreal)
        tempreal=jprobs(t)
        island_assignment_do: &
        DO zcnt=1,zpts
            IF (tempreal<=znewcdf(zcnt)) THEN
                island_sim(t)=zcnt
                EXIT island_assignment_do
            ELSE IF (tempreal>1.0) THEN
                island_sim(t)=zpts
                EXIT island_assignment_do
            END IF
        END DO island_assignment_do



  !-------------> GOTO END OF SURVIVAL IF

ELSE survival_if        ! you're a survivor!!!   -you're taken through the motions


age_sim(t)=age_sim(t-1)+1

    !----------------------------------
    !   HUMAN CAPITAL UPGRADING (DOWNGRADING TOO!), AND ISLAND-SHOCK (which updates island_sim(t-1)!!!! because island_sim(t) is the newly reallocated island)
    !----------------------------------

    ! human capital upgrading/downgrading
    IF(e_sim(t-1)==0 .AND. (skilldep==0.0_8 .OR. hcind_sim(t-1)<=1)) THEN
        hcind_sim(t)=hcind_sim(t-1)
    ELSE IF (e_sim(t-1)==0 .AND. (skilldep>0.0_8 .AND. hcind_sim(t-1)>1)) THEN
        IF (sepprobs(t)<=skilldep_cutoff) THEN
            hcind_sim(t)=hcind_sim(t-1)
        ELSE IF (sepprobs(t)>=skilldep_cutoff .AND. sepprobs(t)<=1.0_8) THEN
            hcind_sim(t)=hcind_sim(t-1)-1
        END IF
    ELSE IF (e_sim(t-1)==1 .AND. hcind_sim(t-1)>0) THEN
        IF (jprobs(t)<=xtransmatrix(hcind_sim(t-1), hcind_sim(t-1))) THEN
            hcind_sim(t)=hcind_sim(t-1)
        ELSE IF (jprobs(t)<=1.0_8) THEN
            hcind_sim(t)=hcind_sim(t-1)+1
        ELSE
            WRITE(*,*) 'SHOULDNOT ARRIVE    HERE: WORKRANDOM1(t)/sepprobs=', sepprobs(t)
            !STOP
        END IF
    ELSE
         WRITE(*,*) 'hcind_sim update mistake, hcind_sim, e_sim', hcind_sim(t-1), e_sim(t-1)
            !STOP
    END IF

    !-----------------------------------------
    !   ISLAND PRODUCTIVITY SHOCK
    !-----------------------------------------

    ! island shock hits island_sim(t-1)


    ! check
    tempreal=zprobs(t)
    IF (ztransmatrix(island_sim(t-1), island_sim(t-1))>=zprobs(t)) THEN

        island_sim(t-1)=island_sim(t-1)
        flag_z=1

    ELSE  IF (ztransmatrix(island_sim(t-1), island_sim(t-1)) < zprobs(t)) THEN

        zprobs(t)=zprobs(t)-ztransmatrix(island_sim(t-1), island_sim(t-1))

        IF(zprobs(t)<0.0) WRITE(10,*) 'ALREADY ERROR: diminished zprobs, flag============='

        tempint=island_sim(t-1)
        tempint2=island_sim(t-1)
        flag_z=0

        island_assignment_do2: DO zcnt=1, zpts
            IF(zprobs(t)<0.0) WRITE(10,*) 'ERROR: diminished zprobs, flag=',flag_z
            IF(island_sim(t-1)-zcnt>0 .AND. flag_z==0) THEN

                tempint=island_sim(t-1)-zcnt

                IF(ztransmatrix(island_sim(t-1), tempint)>= zprobs(t)) THEN
                    island_sim(t-1)=tempint
                    flag_z=1
                    EXIT island_assignment_do2
                ELSE
                    zprobs(t)=zprobs(t)-ztransmatrix(island_sim(t-1), tempint)
                END IF
            ELSE
                !WRITE(10,*) 'weird:check... flag_z=', flag_z, 'island_sim=', island_sim(t-1), 'zcnt=', zcnt
            END IF

            IF(island_sim(t-1)+zcnt<=zpts .AND. flag_z==0)  THEN
                IF(zprobs(t)<0.0) WRITE(10,*) 'ERROR: diminished zprobs, flag=',flag_z

                tempint2=island_sim(t-1)+zcnt

                IF(ztransmatrix(island_sim(t-1), tempint2)>zprobs(t)-0.00001_8) THEN
                    island_sim(t-1)=tempint2
                    flag_z=1
                    EXIT island_assignment_do2
                ELSE
                    zprobs(t)=zprobs(t)-ztransmatrix(island_sim(t-1), tempint2)
                END IF
            END IF


            IF(tempint==1 .AND. tempint2==zpts) THEN
!			WRITE(10,*) 'ERRRRRRRRORRRRRRRR in islandsim, flag=', flag_z, 'zprobs=', zprobs(t)!
!			WRITE(10,*) 'original zprobs(t)=', tempreal, 'island_sim(t-1)=', island_sim(t-1), '|island_sim(t)=', island_sim(t)

	    END IF
        END DO island_assignment_do2

    END IF


    ! assign updated island_sim to the today's ex post island indicator
    island_sim(t)=island_sim(t-1)       ! workers stays on the same island, but island moves according to islandprod_ind
    IF(island_sim(t)<0) WRITE(10,*) 'island trouble: island_sim(t) gives a below-zero island indicator'



    !------------------------------------------------------------------------
    !  SEPARATION STAGE IN CASE OF EMPLOYED, REALLOCATION+MATCHING IN CASE OF UNEMPLOYED
    !------------------------------------------------------------------------


    ! ---- sep: lose the job
    sep_or_matching:&
        IF (e_sim(t-1)==1 .AND. (sepprobs(t)>=sep_cutoff_vec(MAX(hcind_sim(t),1)) .OR. poliDW(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))==0)) THEN

                duration_sim(t)=0
                hcten_sim(t)=hcten_sim(t-1)
                e_sim(t)=0
                wage_sim(t)=0.0_8


        !----keep the job
        ELSE IF (e_sim(t-1)==1 .AND. (sepprobs(t)<=sep_cutoff_vec(MAX(hcind_sim(t),1)) .AND. poliDW(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))==1)) THEN
                ! keep the job

                hcten_sim(t)=hcten_sim(t-1)+1           !
                e_sim(t)=1              ! KEEP THE JOB
                wage_sim(t)=wage(aggprod_ind(t), hcind_sim(t), island_sim(t))
                e_duration_sim(t)=e_duration_sim(t-1)+1

                ! keep the duration of the last completed unemployment spell
                duration_sim(t)=duration_sim(t-1)

                IF(hcind_sim(t)<1) THEN
                    WRITE(*,*) 'error in hcind_sim assignment'
                    STOP
                END IF



        !----- reallocate

        ELSE IF (e_sim(t-1)==0 .AND. poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))==0) THEN

        ! reallocation unemployment

        !IF (t>tmin_sim2 .AND. (t/4)*4-t==0 .AND. unempdur_estatus(t/4)>=1) THEN
        !    up_reall_distribution_sim(aggprod_ind(t))=up_reall_distribution_sim(aggprod_ind(t))+1.0_8
        !    IF(age_sim(t)<youngcutoff) up_young_reall_distribution_sim(aggprod_ind(t))=up_young_reall_distribution_sim(aggprod_ind(t))+1.0_8
        !    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
        !                                         up_prime_reall_distribution_sim(aggprod_ind(t))=up_prime_reall_distribution_sim(aggprod_ind(t))+1.0_8
        !END IF

        !island_sim(t)=MIN(INT(sepprobs(t)*REAL(maxislands))+1, maxislands)

        !sepprobs(t)=(sepprobs(t)-dd)/(1.0_8-dd)         ! using sepprobs (can be used scaling znewcdf, not needing to rescale sepprobs)
                                                        ! not using jprobs at this step, can be used for occupational direction in future version
                                                        ! using jprobs now, because sepprobs used in skilldep

        island_assignment_do0: DO zcnt=1,zpts
            IF (jprobs(t)<znewcdf(zcnt)) THEN
                island_sim(t)=zcnt
                EXIT island_assignment_do0
            ELSE IF (jprobs(t)>1.0) THEN
                island_sim(t)=zpts
                EXIT island_assignment_do0
            END IF
        END DO island_assignment_do0


        ! stay if new island is worse
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        IF (island_sim(t)<=island_sim(t-1)) THEN

                    island_sim(t)=island_sim(t-1)
                    e_sim(t)=0

                    ! update occupational tenure if worker had a job before
                    IF(hcind_sim(t-1)>0) THEN
                        hcten_sim(t)=hcten_sim(t-1)         ! occupational tenure only counted when at work +1
                    ELSE
                        hcten_sim(t)=0
                    END IF

                    duration_sim(t)=duration_sim(t-1)+1



        ! move if island is better
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ELSE    ! actual change of attention to a different island

                e_sim(t)=0
                IF(hcind_sim(t)>-1) THEN
                        hcind_sim(t)=0
                ELSE
                        hcind_sim(t)=hcind_sim(t)          ! changes to 1, if getting a job
                END IF
                hcten_sim(t)=0
                duration_sim(t)=duration_sim(t-1)+1
        END IF


        !================================
        !------successful matching
        !==================================
                ! lots of updating follows

        ELSE IF (e_sim(t-1)==0 .AND. poliR(aggprod_ind(t), MAX(hcind_sim(t),1),island_sim(t))==1 &
                        .AND. jprobs(t)<=jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))) THEN


        !IF (t>tmin_sim2 .AND. (t/4)*4-t==0) THEN
        !    up_search_distribution_sim(aggprod_ind(t))=up_search_distribution_sim(aggprod_ind(t))+1.0_8
        !    IF(age_sim(t)<youngcutoff) up_young_search_distribution_sim(aggprod_ind(t))=up_search_distribution_sim(aggprod_ind(t))+1.0_8
        !    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
        !                                         up_prime_search_distribution_sim(aggprod_ind(t))=up_search_distribution_sim(aggprod_ind(t))+1.0_8
        !END IF

            !--------------------------
            ! with occupational mobility before
        IF (hcind_sim(t)<=0) THEN

            hcind_sim(t)=1
            hcten_sim(t)=1

            !---------------
            ! no occupational mobility occurs
        ELSE
            hcten_sim(t)=hcten_sim(t-1)+1
            hcind_sim(t)=MAX(hcind_sim(t-1),1)
        END IF

        ! in all case update employment and wage information
            e_sim(t)=1
            wage_sim(t)=wage(aggprod_ind(t), hcind_sim(t), island_sim(t))
            e_duration_sim(t)=1

            ! keep the duration of the last unemployment spell!!
            duration_sim(t)=duration_sim(t-1)

       !--------unsuccessful matching, remain unemployed
       ELSE IF (e_sim(t-1)==0 .AND. poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))==1 &
                        .AND. jprobs(t)>=jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))) THEN

        !
        !IF (t>tmin_sim2 .AND. (t/4)*4-t==0 .AND. unempdur_estatus(t/4)>=1) THEN
        !    IF (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))>0.00001_8) THEN
        !        up_search_distribution_sim(aggprod_ind(t))=up_search_distribution_sim(aggprod_ind(t))+1.0_8
        !        IF(age_sim(t)<youngcutoff) up_young_search_distribution_sim(aggprod_ind(t))=up_young_search_distribution_sim(aggprod_ind(t))+1.0_8
        !        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
        !                                             up_prime_search_distribution_sim(aggprod_ind(t))=up_prime_search_distribution_sim(aggprod_ind(t))+1.0_8
        !    ELSE IF (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))<=0.00001_8) THEN
        !        up_rest_distribution_sim(aggprod_ind(t))=up_rest_distribution_sim(aggprod_ind(t))+1.0_8
        !        IF(age_sim(t)<youngcutoff) up_young_rest_distribution_sim(aggprod_ind(t))=up_young_rest_distribution_sim(aggprod_ind(t))+1.0_8
        !        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
        !                                             up_prime_rest_distribution_sim(aggprod_ind(t))=up_prime_rest_distribution_sim(aggprod_ind(t))+1.0_8
        !    END IF
        !END IF
        !
                duration_sim(t)=duration_sim(t-1)+1

                IF(hcind_sim(t-1)>0) THEN
                    hcten_sim(t)=hcten_sim(t-1)+1
                ELSE
                    hcten_sim(t)=0
                END IF

                e_sim(t)=0
                wage_sim(t)=0.0_8

       ELSE
                WRITE(*,*) '====================================================================='
                WRITE(*,*) 'contingency should not occur in simulation loop --DUMPING DESCRIPTION'
                WRITE(*,*) ' ** time' , t, 'thread' , threadnumber
                WRITE(*,*) ' ** previous employment status e_sim(t-1)=(1/0 or E/U) -->', e_sim(t-1)
                WRITE(*,FMT=' (A8,F8.5,A6,I4,A5,I4,A5,I4,A5,F10.5)') 'e=0:', jprobs(t),'<>?jf(',aggprod_ind(t),',MAX(',hcind_sim(t),',1),',island_sim(t),')=', &
                                                                                                            jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))
                WRITE(*,*) ' **poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))', poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))

                WRITE(*,*)  ' **(jprobs(t) vs jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))=number)'
                WRITE(*,FMT='(A8,F8.5,A26,I4,A6,F10.5)') 'e=1:', sepprobs(t),'<>?sep_cutoff_vec(MAX(',hcind_sim(t),',1))=', &
                                                                                                                    sep_cutoff_vec(MAX(hcind_sim(t),1))
                WRITE(*,*)  '  ** poliDW(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))', poliDW(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))
                WRITE(*,*)  ' ** zprobs(t)=', zprobs(t)
                WRITE(*,*)  '  THETA  '
                WRITE(*,*)  thetal
                WRITE(*,*) '  sep_cutoff_vec '
                WRITE(*,*)  sep_cutoff_vec
                WRITE(*,*) '====================================================================='
                !! UPDATE VARIABLES: keep in the same position as before, but update tenures
                e_sim(t)=e_sim(t-1)
                IF(e_sim(t-1)==0) THEN
                        wage_sim(t)=0.0_8
                        duration_sim(t)=duration_sim(t-1)+1
                        hcten_sim(t)=hcten_sim(t-1)+1
                ELSE IF (e_sim(t-1)==1) THEN
                        hcten_sim(t)=hcten_sim(t-1)+1           !
                        e_sim(t)=1              ! KEEP THE JOB
                        wage_sim(t)=wage(aggprod_ind(t), hcind_sim(t), island_sim(t))
                        e_duration_sim(t)=e_duration_sim(t-1)+1
                END IF



       END IF sep_or_matching


END IF survival_if
! END OF PERIOD

END DO indiv_timeseries_do






!!---------------------------------------------------
!! OCCUPATIONAL MOBILITY THROUGH UNEMPLOYMENT : assign cocc/nocc indicators
!!----------------------------------------------------

    !! ORIGINAL
    !! occmob_ind=-2 unassigned indicator, should only occur for those born at beginning of panel, without any previous occupation
                    ! and those leaving the panel, no subsequent occupation
    !! occmob_ind=-1 born in middle of panel, no previous occupation
    !! occmob_ind=0 employed
    !! occmob_ind=1 end of spell with occupational change
    !! occmob_ind=2 end of spell without occupational change

    !! CHANGED
    !! occmob_ind=-2 unassigned indicator, should only occur for those born at beginning of panel, without any previous occupation
                    ! and those leaving the panel, no subsequent occupation
    !! occmob_ind=-1 born in middle of panel, no previous occupation
    !! occmob_ind=0 employed
    !! occmob_ind=1 end of spell with occupational change
    !! occmob_ind=2 end of spell without occupational change
    !! occmob_ind=3 employed after occupational change
    !! occmob_ind=4 employed after unemployment spell without occupational change

    !! occmob_ind indicates whether the u-spell ends with occupational change (1), or not (2)
occmob_ind(:)=-2
corr_occmob_ind(:)=-2


!! counter1=
counter1=tmin_sim2+1


!!====================
!! ASSIGN OCCUPATIONAL MOBILITY STATUS
!!====================

    ! NOTE: we assign this to every week, based on weekly jobfinding and separation, and weekly employment status
    ! WARNING: therefore occmob_ind SHOULD NOT BE USED TO INFER MONTHLY EMPLOYMENT STATUS

cocc_loop: DO WHILE (counter1<tmax_sim2-12)


    IF(counter1>tmax_sim2-32) THEN      ! too close to finish
        counter1=counter1+32

    ELSE IF(e_sim(MAX(counter1,1))==0 .AND. e_sim(MAX(counter1-1,1))==1 .AND. counter1<tmax_sim2-32) THEN       ! week-to-week separation
                                                                                                                ! not in the last 8 months of simulated data series

                    !********** TIMEFORWARD DO
                        timeforward_do:&
                        DO t=counter1+1, tmax_sim2

                            IF(e_sim(t)==1) THEN       ! found employment again
                                tempint=t
                                IF(hcind_sim(t-1)>0) THEN           ! same occupation (notice timing)
                                    occmob_ind(counter1:t-1)=2
                                    occmob_ind(t)=4                      ! used to assign occ mobility in the last unemployment spell to employment spell
                                    corr_occmob_ind(counter1:t-1)=2
                                    corr_occmob_ind(t)=4                      ! used to assign occ mobility in the last unemployment spell to employment spell

                                    ! ALLOW MEASUREMENT ERROR

                                    IF(miscoding_estimation_ind==1 .OR. miscoding_parameter_ind==1 ) THEN
                                            ! if below cutoff of measurement error probability, record as a occupational change
                                            ! later can use jprobs for the entire miscoding transition matrix
                                            IF(jprobs(MAX(counter1,1))<miscoding_estimation) THEN


                                                occmob_ind(counter1:t-1)=1
                                                occmob_ind(t)=3                      ! used to assign occ mobility in the last unemployment spell to employment spell
                                            END IF
                                    END IF
                                ELSE IF (hcind_sim(t-1)==0) THEN  ! different occupation
                                    occmob_ind(counter1:t-1)=1
                                    occmob_ind(t)=3                      ! used to assign occ mobility in the last unemployment spell to employment spell
                                    corr_occmob_ind(counter1:t-1)=1
                                    corr_occmob_ind(t)=3                      ! used to assign occ mobility in the last unemployment spell to employment spell


                                ELSE IF (hcind_sim(t-1)<0) THEN
                                    occmob_ind(counter1:t-1)=-1            ! do nothing because the guy changed
                                    corr_occmob_ind(counter1:t-1)=-1            ! do nothing because the guy changed
                                ELSE
                                    occmob_ind(t)=0
                                    corr_occmob_ind(t)=0
                                END IF
                                counter1=t+1
                                EXIT timeforward_do
                            END IF

                            IF (t==tmax_sim2) THEN
                                counter1=t              ! end the whole procedure
                            END IF



                        END DO timeforward_do
                    !****************

    ELSE IF(e_sim(MAX(counter1,1))==1 .AND. e_sim(MAX(counter1-1,1))==1 .AND. counter1<tmax_sim2-32) THEN
        occmob_ind(MAX(counter1,1))=occmob_ind(MAX(counter1-1,1))
        corr_occmob_ind(MAX(counter1,1))=corr_occmob_ind(MAX(counter1-1,1))
        counter1=counter1+1
    ELSE
        counter1=counter1+1
        occmob_ind(counter1-1)=0
        corr_occmob_ind(MAX(counter1,1))=corr_occmob_ind(MAX(counter1-1,1))
    END IF


END DO cocc_loop




    IF((i/1000)*1000==i) THEN
                tempint2=sum(e_sim)
                IF(verboseswitch==1 .AND. verbosesimulation==1) WRITE(*,*) 'simulations sum e_sim', tempint2
    END IF




!==================================
!  ADD OBSERVATIONS FOR TIME SERIES, taking into account time aggregation, pay attention to coding of monthly employment status
!===================================


    !========================================
    ! NOTIONS TO KEEP IN MIND
    !
    ! *Create employment, unemployment and non-particpation indicators
    ! EMPLOYMENT
    !        !*Based on rmesr= monthly labour force indicator
    !        !gen byte empl = 0
    !        !replace empl = 1 if rmesr==1 | rmesr==2 | rmesr==3 | rmesr==4 | rmesr==5
    !        !*1 With a job entire month, worked all weeks.
    !        !*2 With a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
    !        !*3 With job all month, absent from work w/out pay 1+ weeks, absence due to layoff
    !        !*4 With a job at least 1 but not all weeks, no time on layoff and no time looking for work
    !        !*5 With job at least 1 but not all weeks, some weeks on layoff or looking for work
    ! UNEMPLOYMENT PERSISTENT BREAKAGE OF LINK, NEED AT LEAST 4 WEEKS OF UNEMPLOYMENT AT TIME OF MEASUREMENT
    !
    !
    !===================================================================


 prev_empl_ind=0
 unempdur_estatus=-2

                !DO tcnt=MAX((tmin_sim2/12)*12,1), tmax_sim2-60, 12       ! increase in 3month increments, for quarterly timeseries
                 !   DO t=tcnt, tcnt+8, 4                                        !  for each month in a quarter....

DO tcnt=13, tmax_sim2-60, 12       ! increase in 3month increments, for quarterly timeseries
DO t=tcnt, tcnt+8, 4                                        !  for each month in a quarter....

    !==============================
    ! MONTHLY EMPLOYMENT STATUS
    !==============================
    !
    ! unempdur_estatus integer variable: MONTHLY aggregate
    ! -1 if agent is just born in the month
    !  0 if employed (employed, if employed at some point in the month)
    !  1+ unemployed, duration

    !! four weeks comparison
    !IF(hcind_sim(t)<0 .OR. hcind_sim(t-3)<0 .OR. hcind_sim(t-2)<0 .OR. hcind_sim(t-1)<0) unempdur_estatus(t/4)=-1
    !IF((unempdur_estatus(t/4) .NE. -1) .AND. (e_sim(t)==1 .OR. e_sim(t-1)==1 .OR. e_sim(t-2)==1 .OR. e_sim(t-3)==1)) unempdur_estatus(t/4)=0
    !! EMPLOYED AS WELL IF CODED EMPLOYED LAST MONTH, AND STILL SOME EMPLOYMENT IN THE LAST FOUR WEEKS (now do not need to separately adjust that, with week 2 coding of E status, yes)
    !    !(...)
    !! UNEMPLOYED, IF ALL WEEKS OF THE MONTH UNEMPLOYED
    !IF((unempdur_estatus(t/4) .NE. -1) .AND. (e_sim(t)==0 .AND. e_sim(t-1)==0 .AND. e_sim(t-2)==0 .AND. e_sim(t-3)==0)) unempdur_estatus(t/4)=1
    !! add duration
    !IF(t>8 .AND. unempdur_estatus(t/4)==1 .AND. unempdur_estatus((t/4)-1)>=1) unempdur_estatus(t/4)=unempdur_estatus((t/4)-1)+1
    !

    ! full month comparison, week 2 measurement, and unemployed if previously out of job for more than a month
    IF(hcind_sim(t)<0 .OR. hcind_sim(t-3)<0 .OR. hcind_sim(t-2)<0 .OR. hcind_sim(t-1)<0) unempdur_estatus(t/4)=-1
    IF((unempdur_estatus(t/4) .NE. -1) .AND. (e_sim(t-1)==1 .OR. e_sim(t)==1 .OR. e_sim(t+1)==1 .OR. e_sim(t+2)==1)) unempdur_estatus(t/4)=0
    ! Also employed still when within 3 weeks of employment before
    IF((unempdur_estatus(t/4) .NE. -1) .AND. (e_sim(t-3)==1 .OR. e_sim(t-2)==1 .OR. e_sim(t-1)==1)) unempdur_estatus(t/4)=0
    ! UNEMPLOYED, IF ALL WEEKS OF THE MONTH UNEMPLOYED, AND IF UNEMPLOYED BEFORE UP TO A MONTH
    IF((unempdur_estatus(t/4) .NE. -1) .AND. (e_sim(t)==0 .AND. e_sim(t-1)==0 .AND. e_sim(t-2)==0 .AND. e_sim(t-3)==0) &
                .AND. e_sim(t+1)==0 .AND. e_sim(t+2)==0) unempdur_estatus(t/4)=1
    ! add duration
    IF(t>8 .AND. unempdur_estatus(t/4)==1 .AND. unempdur_estatus((t/4)-1)>=1) unempdur_estatus(t/4)=unempdur_estatus((t/4)-1)+1


    END DO
    END DO


quarter_do_1: &
DO tcnt=MAX((tmin_sim2/12)*12+1,13), tmax_sim2-60, 12       ! increase in 3month increments, for quarterly timeseries
month_do_1: &
DO t=tcnt, tcnt+8, 4                                        !  for each month in a quarter....

    counter1=MAX((t-tmin_sim2)/12+1,1)

    ! age structure
    IF(age_sim(t)<youngcutoff .AND. age_sim(t)>1) data_young_qtr(counter1)=data_young_qtr(counter1)+1.0_8
    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_prime_qtr(counter1)=data_prime_qtr(counter1)+1.0_8


    !===================
    ! previous employment indicator
    IF (e_sim(t)==1 .OR. e_sim(t-1)==1 .OR. e_sim(t-2)==1 .OR. e_sim(t-3)==1) prev_empl_ind=1
    IF (hcind_sim(t)<0 .OR. hcind_sim(t-3)<0 .OR. hcind_sim(t-2)<0 .OR. hcind_sim(t-1)<0) prev_empl_ind=0

    IF(t>tmin_sim2 ) THEN
        !IF(e_sim(t)==0) uallp_distribution_sim(aggprod_ind(t))=uallp_distribution_sim(aggprod_ind(t))+1.0_8
        IF(e_sim(t)==1) eallp_distribution_sim(aggprod_ind(t))=eallp_distribution_sim(aggprod_ind(t))+1.0_8
    END IF


    !===================================
    ! in employment (NEW ACCOUNTING):
    !IF((e_sim(t)==1 .OR. e_sim(t-3)==1 .OR. e_sim(t-2)==1 .OR. e_sim(t-1)==1)  .AND. age_sim(t)>=4) THEN
    IF(unempdur_estatus(t/4)==0) THEN
       data_e_qtr(counter1)=data_e_qtr(counter1)+1.0_8
       ep_distribution_sim(aggprod_ind(t))=ep_distribution_sim(aggprod_ind(t))+1.0_8
                    IF(age_sim(t)<youngcutoff .AND. age_sim(t)>1) data_e_young_qtr(counter1)=data_e_young_qtr(counter1)+1.0_8
                    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_e_prime_qtr(counter1)=data_e_prime_qtr(counter1)+1.0_8
    END IF

    ! separations (make sure that the worker is at least one full month unemployed, before counted -- coming from a month in which he had at least one week of employment!)
    IF(unempdur_estatus((t/4))==0 .AND. unempdur_estatus((t+4)/4)==1 .AND. hcind_sim(t+4)>=0) THEN         ! make sure of survival (implied twice here)
            data_sep_qtr(counter1)=data_sep_qtr(counter1)+1.0_8
            IF(age_sim(t)<youngcutoff)                              data_sep_y_qtr(counter1)=data_sep_y_qtr(counter1)+1.0_8
            IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff)    data_sep_p_qtr(counter1)=data_sep_p_qtr(counter1)+1.0_8
    END IF


    !================
    ! unemployment
    !IF(e_sim(t)==0 .AND. age_sim(t)>5 .AND. ((e_sim(t-3)==0 .AND. e_sim(t-2)==0 .AND. e_sim(t-1)==0) )) THEN

    !! ALL UNEMPLOYED, INCL. ENTRANTS
    IF(unempdur_estatus(t/4)>0 .OR. unempdur_estatus(t/4)<0) THEN
                    data_uall_qtr(counter1)=data_uall_qtr(counter1)+1.0_8
                    !IF(age_sim(t)<youngcutoff .AND. age_sim(t)>1) data_uall_young_qtr(counter1)=data_uall_young_qtr(counter1)+1.0_8
                    !IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_uall_prime_qtr(counter1)=data_uall_prime_qtr(counter1)+1.0_8

                    ! second way of calculating the job finding rate
                    !jf_ave2_counter=jf_ave2_counter+1
                    !IF(unempdur_estatus((t+4)/4)==0) jf_ave2=jf_ave2+1.0_8

    END IF

    !! PREVIOUSLY EMPLOYED BEFORE
    IF(unempdur_estatus(t/4)>0) THEN
                    data_u_qtr(counter1)=data_u_qtr(counter1)+1.0_8
                    IF(age_sim(t)<youngcutoff .AND. age_sim(t)>1) data_u_young_qtr(counter1)=data_u_young_qtr(counter1)+1.0_8
                    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_u_prime_qtr(counter1)=data_u_prime_qtr(counter1)+1.0_8

                    ! second way of calculating the job finding rate
                    jf_ave2_counter=jf_ave2_counter+1
                    IF(unempdur_estatus((t+4)/4)==0) jf_ave2=jf_ave2+1.0_8

    END IF

    ! job finding rate from unemployment (non-entrants)
    IF(unempdur_estatus(t/4)>0 .AND. unempdur_estatus((t+4)/4)==0) THEN
                    data_jf_qtr(counter1)=data_jf_qtr(counter1)+1.0_8

                    IF(age_sim(t)<youngcutoff) THEN
                        data_jf_young_qtr(counter1)=data_jf_young_qtr(counter1)+1.0_8
                    END IF
                    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                        data_jf_prime_qtr(counter1)=data_jf_prime_qtr(counter1)+1.0_8
                    END IF

    END IF

    ! direct method of calculating the unemployment hazards
    IF(t<tmax_sim2-40) THEN
    IF(unempdur_estatus(t/4)>0 .AND. unempdur_estatus(t/4)<=24 .AND. age_sim(t)==age_sim(t+4)-4 ) THEN
                    tot_haz_durationmatrix2(unempdur_estatus(t/4),2)=tot_haz_durationmatrix2(unempdur_estatus(t/4),2)+1.0_8
                    IF (unempdur_estatus((t+4)/4)==0) THEN
                        tot_haz_durationmatrix2(unempdur_estatus(t/4),1)=tot_haz_durationmatrix2(unempdur_estatus(t/4),1)+1.0_8
                    END IF
    END IF
    END IF

    ! direct method of calculating the unemployment survival at inflow, for those that survive at least 32 months when flowing into unemployment
    IF (t<tmax_sim2-130) THEN
    IF(unempdur_estatus((t/4))==0 .AND. unempdur_estatus((t+4)/4)==1 .AND. age_sim(t+128)==age_sim(t)+128 ) THEN
        DO zcnt=1,24
            IF(unempdur_estatus((t+(zcnt*4))/4)==zcnt) THEN
                tot_durationmatrix2(zcnt)=tot_durationmatrix2(zcnt)+1.0_8
            END IF
        END DO
    END IF
    END IF


    ! SINCE data_u_qtr+data_e_qtr covers all workers, apart from birth/retirement, adding them up, should give the entire working population.

    !=================================
    ! unemployment of those who had a job before, not including when unemployed for less than a month
            ! global wavecond " & interview_no2>14 & wave>4 & sample_timetogo>=0"
            ! global sumstats_jf_addcondition " ((unempl_ctv[_n-1]==1 | outlf_ctv[_n-1]==1)  & personkey==personkey[_n-1] & yearmonth==yearmonth[_n-1]+1)  & durdistr_stability==1 & n_spellength[_n-1]<=18 & n_spellength>=1 & (complete_nunspell==1 | incomplete_nunspell==1) "
            ! global ts_u_addcondition " (unempl_ctv==1 | empl_ctv==1) & durdistr_stability==1 ${wavecond} & earlier_empl==1"

        ! we take this as requiring employment within the last 14-46 months, given concavity, approx. on average the last 18 months
    IF(unempdur_estatus(t/4)>0 .AND. prev_empl_ind==1 .AND. duration_sim(t)<=72) THEN
                    data_uadj18m_qtr(counter1)=data_uadj18m_qtr(counter1)+1.0_8
                    IF(age_sim(t)<youngcutoff .AND. age_sim(t)>1) data_uadj18m_young_qtr(counter1)=data_uadj18m_young_qtr(counter1)+1.0_8
                    IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_uadj18m_prime_qtr(counter1)=data_uadj18m_prime_qtr(counter1)+1.0_8
    END IF



    !=======================================
    ! unemployment duration; CPS weekly, use everyone, including those just separated
    IF(e_sim(t)==0 .AND. duration_sim(t)<5) THEN
        data_ult5wk_qtr(counter1)=data_ult5wk_qtr(counter1)+1.0_8
    ELSE IF(e_sim(t)==0 .AND. duration_sim(t)<15) THEN
        data_u5lt15wk_qtr(counter1)=data_u5lt15wk_qtr(counter1)+1.0_8
    ELSE IF(e_sim(t)==0 .AND. duration_sim(t)<27) THEN
        data_u15lt27wk_qtr(counter1)=data_u15lt27wk_qtr(counter1)+1.0_8
    ELSE IF(e_sim(t)==0 .AND. duration_sim(t)>=27) THEN
        data_ugt27wk_qtr(counter1)=data_ugt27wk_qtr(counter1)+1.0_8
    END IF

    !================================
    ! unemployment duration (on-going)
    IF(e_sim(t)==0 .AND. unempdur_estatus(t/4)>=1 .AND. unempdur_estatus(t/4)<=2) THEN
        data_ult3m_qtr(counter1)=data_ult3m_qtr(counter1)+1.0_8
        IF(age_sim(t)<youngcutoff) data_ult3m_yng_qtr(counter1)=data_ult3m_yng_qtr(counter1)+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 data_ult3m_prm_qtr(counter1)=data_ult3m_prm_qtr(counter1)+1.0_8
    END IF
    IF(e_sim(t)==0 .AND. unempdur_estatus(t/4)>=1 .AND. unempdur_estatus(t/4)<=4) THEN
        data_ult5m_qtr(counter1)=data_ult5m_qtr(counter1)+1.0_8
        IF(age_sim(t)<youngcutoff) data_ult5m_yng_qtr(counter1)=data_ult5m_yng_qtr(counter1)+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 data_ult5m_prm_qtr(counter1)=data_ult5m_prm_qtr(counter1)+1.0_8
    END IF
    IF(e_sim(t)==0 .AND. unempdur_estatus(t/4)>=5 .AND. unempdur_estatus(t/4)<=8) THEN
        data_ult9m_qtr(counter1)=data_ult9m_qtr(counter1)+1.0_8
        IF(age_sim(t)<youngcutoff) data_ult9m_yng_qtr(counter1)=data_ult9m_yng_qtr(counter1)+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 data_ult9m_prm_qtr(counter1)=data_ult9m_prm_qtr(counter1)+1.0_8
    END IF

    IF(e_sim(t)==0 .AND. unempdur_estatus(t/4)>=9 .AND. unempdur_estatus(t/4)<=12) THEN
        data_ult13m_qtr(counter1)=data_ult13m_qtr(counter1)+1.0_8
        IF(age_sim(t)<youngcutoff) data_ult13m_yng_qtr(counter1)=data_ult13m_yng_qtr(counter1)+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 data_ult13m_prm_qtr(counter1)=data_ult13m_prm_qtr(counter1)+1.0_8
    END IF

    IF(e_sim(t)==0 .AND. unempdur_estatus(t/4)>=13 .AND. unempdur_estatus(t/4)<=18) THEN
        data_ugt13m_qtr(counter1)=data_ugt13m_qtr(counter1)+1.0_8
        IF(age_sim(t)<youngcutoff) data_ugt13m_yng_qtr(counter1)=data_ugt13m_yng_qtr(counter1)+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 data_ugt13m_prm_qtr(counter1)=data_ugt13m_prm_qtr(counter1)+1.0_8
    END IF

    IF(t>tmin_sim2) THEN
        IF(e_sim(t)==0) uallp_distribution_sim(aggprod_ind(t))=uallp_distribution_sim(aggprod_ind(t))+1.0_8
        !IF(e_sim(t)==1) eallp_distribution_sim(aggprod_ind(t))=eallp_distribution_sim(aggprod_ind(t))+1.0_8
    END IF
    ! for unemployment distribution: aggregate productivity
    IF(unempdur_estatus(t/4)>=1) THEN

        up_distribution_sim(aggprod_ind(t))=up_distribution_sim(aggprod_ind(t))+1.0_8
            IF(age_sim(t)<youngcutoff) up_young_distribution_sim(aggprod_ind(t))=up_young_distribution_sim(aggprod_ind(t))+1.0_8
            IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 up_prime_distribution_sim(aggprod_ind(t))=up_prime_distribution_sim(aggprod_ind(t))+1.0_8


        IF (poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t)) == 1) THEN

            IF (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))>0.00001_8) THEN
                up_search_distribution_sim(aggprod_ind(t))=up_search_distribution_sim(aggprod_ind(t))+1.0_8
                IF(age_sim(t)<youngcutoff) up_young_search_distribution_sim(aggprod_ind(t))=up_young_search_distribution_sim(aggprod_ind(t))+1.0_8
                IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                     up_prime_search_distribution_sim(aggprod_ind(t))=up_prime_search_distribution_sim(aggprod_ind(t))+1.0_8
            ELSE IF (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t))<=0.00001_8) THEN
                up_rest_distribution_sim(aggprod_ind(t))=up_rest_distribution_sim(aggprod_ind(t))+1.0_8
                IF(age_sim(t)<youngcutoff) up_young_rest_distribution_sim(aggprod_ind(t))=up_young_rest_distribution_sim(aggprod_ind(t))+1.0_8
                IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                     up_prime_rest_distribution_sim(aggprod_ind(t))=up_prime_rest_distribution_sim(aggprod_ind(t))+1.0_8
            END IF

        ELSE IF (poliR(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t))==0) THEN
            up_reall_distribution_sim(aggprod_ind(t))=up_reall_distribution_sim(aggprod_ind(t))+1.0_8
            IF(age_sim(t)<youngcutoff) up_young_reall_distribution_sim(aggprod_ind(t))=up_young_reall_distribution_sim(aggprod_ind(t))+1.0_8
            IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                                 up_prime_reall_distribution_sim(aggprod_ind(t))=up_prime_reall_distribution_sim(aggprod_ind(t))+1.0_8
        END IF

    END IF



    ! for unemployment distribution: aggregate productivity, and island location
    IF(unempdur_estatus(t/4)>=1) THEN

                upz_distribution_sim(aggprod_ind(t), island_sim(t))=upz_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
                IF(hcind_sim(t)>=1 .AND. hcind_sim(t)<=xpts) &
                upz_x_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))=upz_x_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8

                IF(age_sim(t)<youngcutoff) THEN
                    upz_young_distribution_sim(aggprod_ind(t), island_sim(t))=upz_young_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
                    IF(hcind_sim(t)>=1 .AND. hcind_sim(t)<=xpts) &
                    upz_x_young_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))=upz_x_young_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8

                ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    upz_prime_distribution_sim(aggprod_ind(t), island_sim(t))=upz_prime_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
                    IF(hcind_sim(t)>=1 .AND. hcind_sim(t)<=xpts) &
                    upz_x_prime_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))=upz_x_prime_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8

                END IF
    END IF






    !vacancies (count all, including those for workers only 1 week unemployed)
    IF(e_sim(t)==0 .AND. nosearch_withvacancy_ind .ne. 1) data_v_qtr (counter1)=data_v_qtr(counter1)+ (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t)))**(1.0_8/eta)
    IF(e_sim(t)==0 .AND. nosearch_withvacancy_ind==1) data_v_qtr (counter1)=data_v_qtr(counter1)+ (jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t)))

    IF(e_sim(t)==0) tot_costofhires=tot_costofhires+k*(jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t)))**(1.0_8/eta)
    ! hires here use expected value ...
    !IF(e_sim(t)==0) tot_hires=tot_hires+(jf(aggprod_ind(t),MAX(hcind_sim(t),1),island_sim(t)))
    ! count as hires those actually hired for one month or more? NOW, even 1 week of work is a hire...
    IF(e_sim(t)==0 .AND. e_sim(t+1)==1) tot_hires=tot_hires+1.0_8


    !productivity and wages: COUNT EVERYONE
    IF(e_sim(t)==1) THEN
        data_prod_qtr(counter1)=data_prod_qtr(counter1)+ prod(aggprod_ind(t), hcind_sim(t), island_sim(t))
        data_wage_qtr(counter1)=data_wage_qtr(counter1)+ wage_sim(t)
        logwage_disp_all=logwage_disp_all+log(wage_sim(t))**2.0_8
        logwage_ave_all=logwage_disp_all+log(wage_sim(t))
        logwage_all_counter=logwage_all_counter+1
        ! high occ tenure
        IF(hcten_sim(t) >= 480 .AND. hcten_sim(t) <= 1920) THEN
        logwage_disp_highoccten=logwage_disp_highoccten+log(wage_sim(t))**2.0_8
        logwage_ave_highoccten=logwage_ave_highoccten+log(wage_sim(t))
        logwage_highoccten_counter=logwage_highoccten_counter+1
        ! newly hired wages, measured at moment worker is hired...
        END IF
    END IF


    ! for employment distribution: aggregate productivity, and island location
    IF(e_sim(t)==1) THEN

        epz_distribution_sim(aggprod_ind(t), island_sim(t))=epz_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
        IF(age_sim(t)<youngcutoff) &
                    epz_young_distribution_sim(aggprod_ind(t), island_sim(t))=epz_young_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                    epz_prime_distribution_sim(aggprod_ind(t), island_sim(t))=epz_prime_distribution_sim(aggprod_ind(t), island_sim(t))+1.0_8
        wagepz_distribution_sim(aggprod_ind(t), island_sim(t))=wagepz_distribution_sim(aggprod_ind(t), island_sim(t))+wage_sim(t)
        IF(age_sim(t)<youngcutoff) &
                    wagepz_young_distribution_sim(aggprod_ind(t), island_sim(t))=wagepz_young_distribution_sim(aggprod_ind(t), island_sim(t))+wage_sim(t)
        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                    wagepz_prime_distribution_sim(aggprod_ind(t), island_sim(t))=wagepz_prime_distribution_sim(aggprod_ind(t), island_sim(t))+wage_sim(t)

        IF(hcind_sim(t)>=1 .AND. hcind_sim(t)<=xpts) THEN
            epz_x_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))=epz_x_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8
            IF(age_sim(t)<youngcutoff) epz_x_young_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))=epz_x_young_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8
            IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) epz_x_prime_distribution_sim(aggprod_ind(t), hcind_sim(t),island_sim(t))=epz_x_prime_distribution_sim(aggprod_ind(t), hcind_sim(t), island_sim(t))+1.0_8

            IF (age_sim(t)>48 .AND. e_sim(MAX(1,t-48))==1) THEN

                IF(unempdur_estatus((t/4))==0) THEN
                epz_1yb4_x_distribution_sim(aggprod_ind(t), hcind_sim(MAX(1,t-48)), island_sim(MAX(1,t-48)))=epz_x_distribution_sim(aggprod_ind(t), hcind_sim(MAX(1,t-48)), island_sim(MAX(1,t-48)))+1.0_8
                    IF(unempdur_estatus((t+4)/4)==1 .AND. hcind_sim(t+4)>=0) THEN
                    sep_epz_1yb4_x_distribution_sim(aggprod_ind(t), hcind_sim(MAX(1,t-48)), island_sim(MAX(1,t-48)))=sep_epz_1yb4_x_distribution_sim(aggprod_ind(t), hcind_sim(MAX(1,t-48)), island_sim(MAX(1,t-48)))+1.0_8
                    END IF
                END IF

            END IF

        END IF
    END IF


    !!!!! REALLOCATION THROUGH UNEMPLOYMENT
    !!!!! --- REALLOCATION ASSIGNED STATISTICS

    IF( unempdur_estatus(t/4)>=1 ) THEN

        ! cocc

        IF(occmob_ind(t)==1) data_cocc_qtr(counter1)=data_cocc_qtr(counter1)+1.0_8
        IF(occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) data_cocc_young_qtr(counter1)=data_cocc_young_qtr(counter1)+1.0_8
        IF(occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_cocc_prime_qtr(counter1)=data_cocc_prime_qtr(counter1)+1.0_8

        ! nocc
        IF(occmob_ind(t)==2) data_nocc_qtr(counter1)=data_nocc_qtr(counter1)+1.0_8
        IF(occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) data_nocc_young_qtr(counter1)=data_nocc_young_qtr(counter1)+1.0_8
        IF(occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_nocc_prime_qtr(counter1)=data_nocc_prime_qtr(counter1)+1.0_8

        ! pocc: job found from unemployment, with occupational change
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==1) THEN
                            data_pocc_qtr(counter1)=data_pocc_qtr(counter1)+1.0_8
                            !  (censoring in the SIPP! -- taken into account in MLEV MEASURES BELOW)
                            !data_ucompldur_occ_qtr(counter1)=data_ucompldur_occ_qtr(counter1)+unempdur_estatus(t/4)

                            !! DISTRIBUTION OF COMPLETED SPELLS (used to calculate restricted)
                            IF(unempdur_estatus(t/4)<=18 .AND. unempdur_estatus(t/4)>0) THEN
                            data_ucompldur_occ_qtr(counter1)=data_ucompldur_occ_qtr(counter1)+unempdur_estatus(t/4)
                            data_compduration_cocc_qtr(counter1,unempdur_estatus(t/4))=data_compduration_cocc_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                            data_compduration_cocc(unempdur_estatus(t/4))=data_compduration_cocc(unempdur_estatus(t/4))+1.0_8
                                IF(age_sim(t)<youngcutoff) THEN
                                    data_compduration_cocc_young_qtr(counter1,unempdur_estatus(t/4))=data_compduration_cocc_young_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                                    data_compduration_cocc_young(unempdur_estatus(t/4))=data_compduration_cocc_young(unempdur_estatus(t/4))+1.0_8
                                ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                                    data_compduration_cocc_prime_qtr(counter1,unempdur_estatus(t/4))=data_compduration_cocc_prime_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                                    data_compduration_cocc_prime(unempdur_estatus(t/4))=data_compduration_cocc_prime(unempdur_estatus(t/4))+1.0_8
                                END IF
                            END if
                IF(age_sim(t)<youngcutoff) THEN
                            data_pocc_young_qtr(counter1)=data_pocc_young_qtr(counter1)+1.0_8
                            data_ucompldur_young_occ_qtr(counter1)=data_ucompldur_young_occ_qtr(counter1)+unempdur_estatus(t/4)
                ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                            data_pocc_prime_qtr(counter1)=data_pocc_prime_qtr(counter1)+1.0_8
                            data_ucompldur_prime_occ_qtr(counter1)=data_ucompldur_prime_occ_qtr(counter1)+unempdur_estatus(t/4)
                END IF
        END IF


        ! pnocc
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==2) THEN
                            data_pnocc_qtr(counter1)=data_pnocc_qtr(counter1)+1.0_8
                            !data_ucompldur_nocc_qtr(counter1)=data_ucompldur_nocc_qtr(counter1)+unempdur_estatus(t/4)

                            IF(unempdur_estatus(t/4)<=18 .AND. unempdur_estatus(t/4)>0) THEN
                            data_ucompldur_nocc_qtr(counter1)=data_ucompldur_nocc_qtr(counter1)+unempdur_estatus(t/4)
                            data_compduration_nocc_qtr(counter1,unempdur_estatus(t/4))=data_compduration_nocc_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                            data_compduration_nocc(unempdur_estatus(t/4))=data_compduration_nocc(unempdur_estatus(t/4))+1.0_8
                                IF(age_sim(t)<youngcutoff) THEN
                                    data_compduration_nocc_young_qtr(counter1,unempdur_estatus(t/4))=data_compduration_nocc_young_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                                    data_compduration_nocc_young(unempdur_estatus(t/4))=data_compduration_nocc_young(unempdur_estatus(t/4))+1.0_8
                                ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                                    data_compduration_nocc_prime_qtr(counter1,unempdur_estatus(t/4))=data_compduration_nocc_prime_qtr(counter1,unempdur_estatus(t/4))+1.0_8
                                    data_compduration_nocc_prime(unempdur_estatus(t/4))=data_compduration_nocc_prime(unempdur_estatus(t/4))+1.0_8
                                END IF
                            END if

                IF(age_sim(t)<youngcutoff) THEN
                            data_pnocc_young_qtr(counter1)=data_pnocc_young_qtr(counter1)+1.0_8
                            data_ucompldur_young_nocc_qtr(counter1)=data_ucompldur_young_nocc_qtr(counter1)+unempdur_estatus(t/4)
                ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                            data_pnocc_prime_qtr(counter1)=data_pnocc_prime_qtr(counter1)+1.0_8
                            data_ucompldur_prime_nocc_qtr(counter1) =data_ucompldur_prime_nocc_qtr(counter1)+unempdur_estatus(t/4)
                END IF
        END IF


        ! cocc INFLOW, measured at first moment of unemployment; take those who have at least two months of unemployment
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==1) data_cocc_inflow_qtr(counter1)=data_cocc_inflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) data_cocc_young_inflow_qtr(counter1)=data_cocc_young_inflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_cocc_prime_inflow_qtr(counter1)=data_cocc_prime_inflow_qtr(counter1)+1.0_8

        ! nocc
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==2) data_nocc_inflow_qtr(counter1)=data_nocc_inflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) data_nocc_young_inflow_qtr(counter1)=data_nocc_young_inflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t-4)/4)==0 .AND. occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_nocc_prime_inflow_qtr(counter1)=data_nocc_prime_inflow_qtr(counter1)+1.0_8

    ! cocc OUTFLOW, measured at last moment of unemployment
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==1) data_cocc_outflow_qtr(counter1)=data_cocc_outflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) data_cocc_young_outflow_qtr(counter1)=data_cocc_young_outflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_cocc_prime_outflow_qtr(counter1)=data_cocc_prime_outflow_qtr(counter1)+1.0_8
    ! nocc
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==2) data_nocc_outflow_qtr(counter1)=data_nocc_outflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) data_nocc_young_outflow_qtr(counter1)=data_nocc_young_outflow_qtr(counter1)+1.0_8
        IF(unempdur_estatus((t+4)/4)==0 .AND. occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_nocc_prime_outflow_qtr(counter1)=data_nocc_prime_outflow_qtr(counter1)+1.0_8
    END IF


    !---- subsequent reallocations as a function of previous unemployment spell
    !   ----  in the data we only see this for those people who have multiple unemployment spells within 4 years
    !   ----  if exogenous breakups are very rare, and a large proportion of measured breakups are endogenous, we have to reproduce these conditions in the program!!!
    !   ----  this means adding a condition that previous unemployment and employment spell, plus current unemployment spell fall with blocks of 180 weeks.
    !   ----  this condition is marked by <-----***** on the RHS margin below



    ! --- in DATA:
    !               * spells that complete in 18 months or less
    !               * measure at outflow moment
    !               * cumulative profile. consider at duration x, if completed duration y >= x, y<=18



    ! CPS duration distribution
     ! duration distribution
    unemp_dur_if: IF(e_sim(t)==0) THEN
    IF(duration_sim(t)>=0) THEN
    IF(e_sim(MAX(t-duration_sim(t)-4,1))==1) THEN          ! check uninterrupted unemployment spell


          ! FOR CPS DURATION DISTRIBUTION
          IF(duration_sim(t)>0 .AND. duration_sim(t)<5) ult5wk=ult5wk+1.0_8
          IF(duration_sim(t)>=5 .AND. duration_sim(t)<15) u5lt15wk=u5lt15wk+1.0_8
          IF(duration_sim(t)>=15 .AND. duration_sim(t)<27) u15lt27wk=u15lt27wk+1.0_8
          IF(duration_sim(t)>=27) ugt27wk=ugt27wk+1.0_8

                          !! first month at the 0 element of the tot_durationmatrices!!!!
                          !tot_durationmatrix(MIN((MAX(duration_sim(t),0)/4),24))=tot_durationmatrix(MIN((MAX(duration_sim(t),0)/4),24))+1.0_8
                          !IF(age_sim(t)<youngcutoff) THEN
                          !    tot_durationmatrix_young(MIN((MAX(duration_sim(t),0)/4),24))=tot_durationmatrix_young(MIN((MAX(duration_sim(t),0)/4),24))+1.0_8
                          !ELSE IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                          !    tot_durationmatrix_prime(MIN((MAX(duration_sim(t),0)/4),24))=tot_durationmatrix_prime(MIN((MAX(duration_sim(t),0)/4),24))+1.0_8
                          !END IF
                          !
                          !
                          !  ! DURATION PROFILE
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2) tot_durationmatrix_cocc(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_cocc(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !! test that the durationmatrix_cocc with duration<4 counts the same as cocc_inflow
                          !
                          !! DISTRIBUTION OVER TIME
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2 .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1)  data_totduration_cocc_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_cocc_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
                          !
                          !!IF(occmob_ind(t)==1 .AND. duration_sim(t)<4) data_temp_qtr(counter1)=data_temp_qtr(counter1)+1.0_8
                          !        ! conclusion: almost the same, sometimes a small difference.
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_cocc_young(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_cocc_young(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_cocc_prime(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_cocc_prime(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1) &
                          !      data_totduration_cocc_young_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_cocc_young_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
                          !IF(occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1) &
                          !      data_totduration_cocc_prime_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_cocc_prime_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
                          !
                          !! nocc
                          !IF(occmob_ind(t)==2  .AND. age_sim(t)<highcutoff2)  tot_durationmatrix_nocc(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_nocc(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !! test that the durationmatrix_cocc with duration<4 counts the same as cocc_inflow
                          !IF(occmob_ind(t)==2 .AND. age_sim(t)<highcutoff2 .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1)  data_totduration_nocc_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_nocc_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
                          !
                          !!IF(occmob_ind(t)==2 .AND. duration_sim(t)<4) data_temp_qtr2(counter1)=data_temp_qtr2(counter1)+1.0_8
                          !        ! conclusion: almost the same, sometimes a small difference.
                          !IF(occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_nocc_young(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_nocc_young(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !IF(occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_nocc_prime(MIN((duration_sim(t)-1)/4,24))=tot_durationmatrix_nocc_prime(MIN((duration_sim(t)-1)/4,24))+1.0_8
                          !
                          !IF(occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1) &
                          !      data_totduration_nocc_young_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_nocc_young_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
                          !IF(occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff .AND. e_sim(MIN(t+(60-duration_sim(t)),tmax_sim2-12))==1) &
                          !      data_totduration_nocc_prime_qtr(counter1,MIN((duration_sim(t)-1)/4,18))=data_totduration_nocc_prime_qtr(counter1,MIN((duration_sim(t)-1)/4,18))+1.0_8
    END IF
    END IF
    END IF unemp_dur_if


    !=============================
    ! OCCMOB U DURATION PROFILE

    occmob_dur_if: IF(unempdur_estatus(t/4)>=1) THEN           ! UNEMPLOYED

    !!===================================
    !! COMPLETED SPELL DURATIONS  ---> IMPLIED SURVIVAL DISTRIBUTION
    !!===================================

    ! measured and assigned at moment of outflow into employment, as in the SIPP data
    u_outflow_next_period_1_if: &
        IF(unempdur_estatus((t+4)/4)==0) THEN               ! OUTFLOW IN THE NEXT PERIOD



        !---------------------
        ! ALL, spells up to 24 months distinguished; all spells included!
                    ! NOTE: this adds the survival to all previous spell durations
                    ! NOTE: for symmetry with SIPP, require that worker survives at least until 32 months after begin spell!

            DO tcnt2=1, MIN(unempdur_estatus(t/4),24)
                IF(age_sim(t)<highcutoff2) tot_durationmatrix(tcnt2)=tot_durationmatrix(tcnt2)+1.0_8
                IF(age_sim(t)<youngcutoff) tot_durationmatrix_young(tcnt2)=tot_durationmatrix_young(tcnt2)+1.0_8
                IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_prime(tcnt2)=tot_durationmatrix_prime(tcnt2)+1.0_8
                
                IF(unempdur_estatus(t/4)<=40) THEN

                    IF (occmob_ind(t)==1) THEN
                        IF(age_sim(t)<highcutoff2) &
                            tot_durationmatrix2_cocc(tcnt2)=tot_durationmatrix2_cocc(tcnt2)+1.0
                        IF(age_sim(t)<youngcutoff) &
                             tot_durationmatrix2_cocc_young(tcnt2)=tot_durationmatrix2_cocc_young(tcnt2)+1.0
                        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                                tot_durationmatrix2_cocc_prime(tcnt2)=tot_durationmatrix2_cocc_prime(tcnt2)+1.0
                    ELSEIF (occmob_ind(t)==2) THEN
                        IF(age_sim(t)<highcutoff2) &
                            tot_durationmatrix2_nocc(tcnt2)=tot_durationmatrix2_nocc(tcnt2)+1.0
                        IF(age_sim(t)<youngcutoff) &
                            tot_durationmatrix2_nocc_young(tcnt2)=tot_durationmatrix2_nocc_young(tcnt2)+1.0
                        IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) &
                            tot_durationmatrix2_nocc_prime(tcnt2)=tot_durationmatrix2_nocc_prime(tcnt2)+1.0
                    END IF
                END IF

            END DO

          !---------------------
          ! OCCUPATIONAL MOVERS, only for completed spells <=18 !!!!
          !---------------------

          complete_within_18m_ifm: &
          IF(unempdur_estatus(t/4)>0 .AND. unempdur_estatus(t/4)<=18) THEN

          ! *******CUMULATIVE PROFILE********
           DO tcnt2=1, MIN(unempdur_estatus(t/4),18)

                  ! t is month before outflow from unemployment
                  ! tcnt2 = incomplete unemployment duration of spell ending at t
                  ! tempint3 = quarter in which incomplete duration tcnt2 is measured

                  !======== OCCUPATIONAL MOBILITY WITH MISCODING

                  ! OVER THE ENTIRE SAMPLE
                  IF(occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2) tot_durationmatrix_cocc(tcnt2)=tot_durationmatrix_cocc(tcnt2)+1.0_8
                  IF(occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_cocc_young(tcnt2)=tot_durationmatrix_cocc_young(tcnt2)+1.0_8
                  IF(occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_cocc_prime(tcnt2)=tot_durationmatrix_cocc_prime(tcnt2)+1.0_8
                    ! stayers
                  IF(occmob_ind(t)==2 .AND. age_sim(t)<highcutoff2) tot_durationmatrix_nocc(tcnt2)=tot_durationmatrix_nocc(tcnt2)+1.0_8
                  IF(occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_nocc_young(tcnt2)=tot_durationmatrix_nocc_young(tcnt2)+1.0_8
                  IF(occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_nocc_prime(tcnt2)=tot_durationmatrix_nocc_prime(tcnt2)+1.0_8

                  ! DISTRIBUTION OVER TIME

                  ! if assigning that to quarter of incomplete duration
                  tempint3=counter1-(unempdur_estatus(t/4)-tcnt2)/3
                  ! if assigning to quarter of completion
                  !tempint3=counter1

                  IF(occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2)  data_totduration_cocc_qtr(tempint3,tcnt2)=data_totduration_cocc_qtr(tempint3,tcnt2)+1.0_8
                  IF(occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) data_totduration_cocc_young_qtr(tempint3,tcnt2)=data_totduration_cocc_young_qtr(tempint3,tcnt2)+1.0_8
                  IF(occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_totduration_cocc_prime_qtr(tempint3,tcnt2)=data_totduration_cocc_prime_qtr(tempint3,tcnt2)+1.0_8
                    ! stayers
                  IF(occmob_ind(t)==2 .AND. age_sim(t)<highcutoff2)  data_totduration_nocc_qtr(tempint3,tcnt2)=data_totduration_nocc_qtr(tempint3,tcnt2)+1.0_8
                  IF(occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) data_totduration_nocc_young_qtr(tempint3,tcnt2)=data_totduration_nocc_young_qtr(tempint3,tcnt2)+1.0_8
                  IF(occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_totduration_nocc_prime_qtr(tempint3,tcnt2)=data_totduration_nocc_prime_qtr(tempint3,tcnt2)+1.0_8


                  !======= CORRECT OCCUPATIONAL MOBILITY

                  ! OVER THE ENTIRE SAMPLE
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2) tot_durationmatrix_cr_cocc(tcnt2)=tot_durationmatrix_cr_cocc(tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_cr_cocc_young(tcnt2)=tot_durationmatrix_cr_cocc_young(tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_cr_cocc_prime(tcnt2)=tot_durationmatrix_cr_cocc_prime(tcnt2)+1.0_8
                    ! stayers
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)<highcutoff2) tot_durationmatrix_cr_nocc(tcnt2)=tot_durationmatrix_cr_nocc(tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) tot_durationmatrix_cr_nocc_young(tcnt2)=tot_durationmatrix_cr_nocc_young(tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) tot_durationmatrix_cr_nocc_prime(tcnt2)=tot_durationmatrix_cr_nocc_prime(tcnt2)+1.0_8

                  ! DISTRIBUTION OVER TIME
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)<highcutoff2)  data_totduration_cr_cocc_qtr(tempint3,tcnt2)=data_totduration_cr_cocc_qtr(tempint3,tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)<youngcutoff) data_totduration_cr_cocc_young_qtr(tempint3,tcnt2)=data_totduration_cr_cocc_young_qtr(tempint3,tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==1 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_totduration_cr_cocc_prime_qtr(tempint3,tcnt2)=data_totduration_cr_cocc_prime_qtr(tempint3,tcnt2)+1.0_8
                    ! stayers
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)<highcutoff2)  data_totduration_cr_nocc_qtr(tempint3,tcnt2)=data_totduration_cr_nocc_qtr(tempint3,tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)<youngcutoff) data_totduration_cr_nocc_young_qtr(tempint3,tcnt2)=data_totduration_cr_nocc_young_qtr(tempint3,tcnt2)+1.0_8
                  IF(corr_occmob_ind(t)==2 .AND. age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) data_totduration_cr_nocc_prime_qtr(tempint3,tcnt2)=data_totduration_cr_nocc_prime_qtr(tempint3,tcnt2)+1.0_8
           END DO
          END IF complete_within_18m_ifm

    END IF u_outflow_next_period_1_if
    END IF occmob_dur_if

END DO month_do_1!t month counter
END DO quarter_do_1 !t  quarter counter
tmax_qtr=counter1




!!!=================================================
!!!------------------------------------------------
!!! SIPP PSEUDO-WINDOWS, FOR REPEAT MOBILITY, REPEAT SEPARATIONS, AND UNEMPLOYMENT PROPORTION!
!!!------------------------------------------------
!!!=================================================


counter1=tmin_sim2+13

!== window length --> tempint               !WINDOW LENGTH
tempint=192  !=4 years

!! CHECK IF WORKER EMPLOYED THROUGHOUT WINDOW


!!========= SIPP WINDOW GENERATION --- DIFFERENT WINDOWS
sippwindow_do: &
DO WHILE (counter1<tmax_sim2-tempint-1)

!! survival within panel
flag_windowsurvival=0
IF((age_sim(counter1)==age_sim(counter1+tempint-1)-(tempint-1)) .AND. (age_sim(counter1)>1)) THEN
    flag_windowsurvival=1
END IF 


!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 1. STEP INSIDE WINDOW:<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!! INITIALIZATION/INITIAL OBS
u_flag=0
e_flag=0
tempcounter=0  !! USED TO COUNT UNEMPLOYMENT SPELLS WITHIN WINDOW
repeatmobspells_matrix=0

IF (unempdur_estatus(counter1/4)==0 .AND. age_sim(counter1)==age_sim(counter1+144)-144) THEN
    counter_u_window_all=counter_u_window_all+1
    IF(age_sim(counter1)<youngcutoff) counter_u_window_all_y=counter_u_window_all_y+1
    IF(age_sim(counter1)>lowcutoff .AND. age_sim(counter1)<highcutoff) counter_u_window_all_p=counter_u_window_all_p+1
    u_flag=1
    END IF


    
    
    
! SURVIVAL WITHIN THE PSEUDO SIPP PANEL: NEED TO CHANGE THIS!!!!
!IF(age_sim(counter1)==age_sim(counter1+tempint)-tempint) THEN
    
!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 2. MOVE WITHIN WINDOW:<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
    
!********************
! >>>>>>>>ENTER WINDOW: t , t+4, t+8 , t+12, t+16,.... counts forwards months 1,2,3,4,5,...<<<<<<<<<
!***********************
inside_pseudo_sipp_window_do: &
DO t=counter1, counter1+tempint, 4

    ! set earlier_employment flag
    IF(unempdur_estatus(t/4)==0) e_flag=1
    IF(unempdur_estatus(t/4)<0) e_flag=0

    !=============================
    !--  REPEAT SEPARATIONS:
    !================================
        !               complete entire spell of 2-10 months, come back in the same occupation, stay in sample for the next 2.5 year (100 weeks) = 0
        !               lose job and find it again, change occupation, change 0 into 1

    repeat_sep_if: &
    IF (t>=counter1+8 .AND. t<=counter1+48 .AND. (unempdur_estatus(t/4)==0) &
                .AND. (unempdur_estatus((t-4)/4)>=2 .AND. unempdur_estatus((t-4)/4)<=10) &
                .AND. occmob_ind(t-4)==2 .AND. age_sim(t)==age_sim(MIN(t+120,tmax_sim2))-120 &
                .AND. age_sim(counter1)==age_sim(t)-t+counter1                    ) THEN

                                    ! eligible case, at moment of job finding;
                                    ! was in sample, at the beginning of panel,
                                    ! will stay in sample for 2.5 year or more


                                        ! Now, have to check two things:
                                        ! i. this person will stay in sample for the next 2.5 yr, i.e. 120 weeks
                                        ! ii. this person will, in fact, go through unemployment again

                sepocc_afternocc_2_5yr_counter=sepocc_afternocc_2_5yr_counter + 1
                sepnocc_afternocc_2_5yr_counter=sepnocc_afternocc_2_5yr_counter + 1
                sep_afternocc_2_5yr_counter=sep_afternocc_2_5yr_counter + 1

                repeat_sep_u_search_do: &
                DO tcnt=t+4, (t+116), 4

                    IF (hcind_sim(tcnt)<0) THEN
                                ! ineligible window, subtract one from the number of eligible spells, since it was added before
                                sepocc_afternocc_2_5yr_counter=sepocc_afternocc_2_5yr_counter - 1
                                sepnocc_afternocc_2_5yr_counter=sepnocc_afternocc_2_5yr_counter - 1
                                sep_afternocc_2_5yr_counter=sep_afternocc_2_5yr_counter - 1
                                EXIT repeat_sep_u_search_do

                    ELSE IF ((unempdur_estatus(tcnt/4)==0) .AND. &
                                unempdur_estatus((tcnt+4)/4)>=1) THEN
                             ! NEW SEPARATION post-job finding, within panel
                                sep_afternocc_2_5yr=sep_afternocc_2_5yr + 1.0_8

                                    ! make sure it this spell completes within the pseudo-panel
                                    compl_occ_sep_within_pspanel: &
                                    DO tcnt2=tcnt+8, t+120, 4
                                        IF (unempdur_estatus((tcnt2)/4)==0 .AND. unempdur_estatus((tcnt2-4)/4)>=1) THEN

                                            IF (occmob_ind(tcnt2-4)==1  )  THEN
                                                sepocc_afternocc_2_5yr=sepocc_afternocc_2_5yr + 1.0_8
                                                IF(unempdur_estatus((tcnt2-4)/4)>=2 ) sepocc_afternocc_2_5yr_2m=sepocc_afternocc_2_5yr_2m + 1.0_8
                                            ELSE IF (occmob_ind(tcnt2-4)==2 )  THEN
                                                sepnocc_afternocc_2_5yr=sepnocc_afternocc_2_5yr + 1.0_8
                                                IF(unempdur_estatus((tcnt2-4)/4)>=2 ) sepnocc_afternocc_2_5yr_2m=sepnocc_afternocc_2_5yr_2m + 1.0_8
                                            END IF

                                            EXIT compl_occ_sep_within_pspanel

                                        END IF
                                    END DO compl_occ_sep_within_pspanel
                                EXIT repeat_sep_u_search_do
                    END IF
                END DO  repeat_sep_u_search_do

    END IF repeat_sep_if


    ! person leaves sample
!    IF (hcind_sim(t)<0) EXIT inside_pseudo_sipp_window_do


    ! UNEMPLOYMENT RATE FOR THOSE WITH EARLIER EMPLOYMENT IN THE FIRST 4 WAVES, i.e. 16 months, OR IN THE SUBSEQUENT PERIOD UNTIL T.
    IF(t>counter1+64 .AND. e_flag==1 .AND. unempdur_estatus(t/4)>=0) THEN
            earlier_e_counter=earlier_e_counter+1
            IF(age_sim(t)<youngcutoff)                              young_earlier_e_counter=young_earlier_e_counter+1
            IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff)    prime_earlier_e_counter=prime_earlier_e_counter+1

            IF(unempdur_estatus(t/4)>=1) THEN
                unemp_earlier_e_ave=unemp_earlier_e_ave+1.0_8
                IF(age_sim(t)<youngcutoff)                              u_earlier_e_young_ave=u_earlier_e_young_ave+1.0_8
                IF(age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff)    u_earlier_e_prime_ave=u_earlier_e_prime_ave+1.0_8
            END IF

    END IF


    IF(t>counter1+64 .AND. e_flag==1 .AND. unempdur_estatus(t/4)<0) warningcounter=warningcounter+1



    ! CALCULATION OF THE UNEMPLOYMENT PROPORTION
    IF(unempdur_estatus(t/4)>=1 .AND. u_flag==1 .AND. t<=counter1+144 .AND. age_sim(counter1)==age_sim(counter1+144)-144) THEN
                ! IF CHANGING THE WINDOW SIZE (144 now), change it here, and above (where u_flag=1 is set)
                ! CALCULATE PROPORTION OF WORKERS WHO BECOME UNEMPLOYED WITHIN A 3/3.5/4 YEAR WINDOW (NOW 3)

        u_flag=2
        counter_u_window=counter_u_window+1
        IF(age_sim(counter1)<youngcutoff) counter_u_window_y=counter_u_window_y+1
        IF(age_sim(counter1)>lowcutoff .AND. age_sim(counter1)<highcutoff) counter_u_window_p=counter_u_window_p+1
    END IF

    !!------------------------------------------------------    
    !! ALTERNATIVE METHOD TO CALCULATE SAM/SAS/MAS/MAM HERE
    !!------------------------------------------------------
    flag_windowsurvival_if: &
    IF(flag_windowsurvival==1) THEN
     
    !! become employed after unemployment
    IF(unempdur_estatus(t/4)==0 .AND. unempdur_estatus((t-4)/4)>0 .AND. t>counter1) THEN
            
        ! add completed spell count
        tempcounter=tempcounter+1
    
        ! add to nocc or occ spell when satisfying the conditions
        IF(occmob_ind(t-4) == 1 .AND. tempcounter>0 .AND. tempcounter<=10) THEN
            repeatmobspells_matrix(tempcounter,1)=unempdur_estatus((t-4)/4)
            repeatmobspells_matrix(tempcounter,2)=0
        ELSE IF(occmob_ind(t-4) == 2 .AND. tempcounter>0 .AND. tempcounter<=10) THEN
            repeatmobspells_matrix(tempcounter,1)=0
            repeatmobspells_matrix(tempcounter,2)=unempdur_estatus((t-4)/4)
        END IF 
    END IF 
    
    !!-------------------
    !! ORIGINAL METHOD TO CALCULATE SAM/SAS/MAS/MAM HERE
    !!-------------------
    
    
    ! whenever there is a second unemployment spell; OCC/NOCC AFTER OCC

    !================
    ! FIRST UNEMPLOYMENT SPELL ENDS AT *t*
    !==================

        ! t will be refer to beginning employment spell, end first unemployment spell before
        ! tcnt               beginning second unemployment spell
        ! tcnt2              end second unemployment spell

    !! end of first unemployment spell
    first_u_spell_ends_if: &
    IF (unempdur_estatus(t/4)==0 .AND. unempdur_estatus((t-4)/4)>0 .AND. age_sim(t)==age_sim(counter1+tempint)-counter1-tempint+t) THEN               ! UNEMPLOYMENT SPELL ENDS, BUT WORKER STAYS IN SAMPLE in the pseudo-panel


                  !===================
                  !  CONCENTRATION OF OCC MOVE/STAY UNEMPLOYMENT   -----> this needs to be redone!!!

                  IF (occmob_ind(t-4)==1) counter_uocc_window_all=counter_uocc_window_all+1
                  IF (occmob_ind(t-4)==2) counter_unocc_window_all=counter_unocc_window_all+1

                  IF(duration_sim(t-4)<7 .AND. duration_sim(t-4)>0) uspellength_ind=1         ! previous unemployment spell "short"
                  IF(duration_sim(t-4)>=7) uspellength_ind=2                                  ! previous unemployment spell "long"

      !=============================
      ! calculate wage dispersion among ALL newly hired, with more than 2 years in sample

            wagedisp_uhires_if: &
            IF(t>counter1+96) THEN
              forward_loop_wages:&
              DO tcnt=t+4, MIN(counter1+tempint-(sample_timetogo_int*4)+1, t+96),4        ! WAGES OF NEWLY HIRED WITHIN 2 YRS
                IF(e_sim(tcnt)==1) THEN
                    logwage_disp_uhires=logwage_disp_uhires+log(wage_sim(tcnt))**2.0_8
                    logwage_ave_uhires=logwage_ave_uhires+log(wage_sim(tcnt))
                    logwage_uhires_counter=logwage_uhires_counter+1
                END IF
              END DO forward_loop_wages
            END IF wagedisp_uhires_if







    !====================
    !  LOCATING THE START OF THE SECOND UNEMPLOYMENT SPELL
    !======================


    !! CONTINUE ONLY IF SURVIVAL THROUGHOUT PANEL
    !flag_windowsurvival_if: &
    !IF(flag_windowsurvival==1) THEN
    

        ! Looking for the repeat unemployment spell forward
        ! t is stopped at end of first unemployment spell. *tcnt* is moving forward
    
      flag3=0
      
      forward_loop2:&
      DO tcnt=t+4, counter1+tempint-(sample_timetogo_int*4)+1,4         ! FIND START OF SUBSEQUENT U SPELL WITHIN WINDOW; WITH SAMPLE-TIMETOGO CONDITION!!!!!

            ! person leaves sample SHOULD NOT OCCUR!!!
            !IF (hcind_sim(tcnt)<0) EXIT XXX inside_pseudo_sipp_window_do


          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !== BEGIN SECOND UNEMPLOYMENT SPELL
          repeat_u_entry_1_if: &
          IF (unempdur_estatus(tcnt/4)>0 .AND. unempdur_estatus((tcnt-4)/4)==0 .AND. occmob_ind(tcnt)>0) THEN            ! BECOME UNEMPLOYED AGAIN (second unemployment spell), with at least sample_timetogo_ind in the remaining window

                IF (occmob_ind(t-4)==1) counter_uocc_window=counter_uocc_window+1     ! interpretation?
                IF (occmob_ind(t-4)==2) counter_unocc_window=counter_unocc_window+1   ! interpretation?

                !============
                ! determine the length of subsequent unemployment, for unemployment duration correlations across spells


                        u_now_loop2:&
                        DO tcnt2=tcnt+4, counter1+tempint, 4             !  search: when does the next job start?

                            IF (hcind_sim(tcnt2)<0) THEN
                                u_spell_now=log(12.0_8)
                                flag3=2
                                tempint2=-1
                                EXIT u_now_loop2
                            END IF 
    
                            IF (e_sim(tcnt2)==1) THEN
                               u_spell_now=log(REAL(MIN((tcnt2-tcnt)/4+1,12)))
                               tempint2=tcnt2
                               flag3=1                    ! set flag, indeed end of spell
                                                           ! FLAG3=1 means that next job (EUEU->E) is within the window
                               EXIT u_now_loop2
                            END IF

                         END DO u_now_loop2

                         IF(flag3==0) THEN
                            u_spell_now=log(12.0_8)
                            flag3=2
                            tempint2=-1
                         END IF

                                                    ! reset flag
                         tcnt2=tempint2             ! time of becoming employed again (end of second unemployment spell)
                         u_spell_before=log(MAX(REAL(INT(duration_sim(t-4)/4)),12.0_8))

       !! for all previous spells
       !         u_spell_now_ave=u_spell_now_ave+(REAL(u_spell_now))
       !         u_spell_before_ave=u_spell_before_ave+(u_spell_before)
       !
       !         u_spell_now_sq=u_spell_now_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_sq=u_spell_before_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm=u_spell_crossterm+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_now_ave=u_spell_now_ave+(REAL(u_spell_now))
       !             u_spell_young_before_ave=u_spell_young_before_ave+(u_spell_before)
       !             u_spell_young_now_sq=u_spell_now_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_sq=u_spell_young_before_sq+(u_spell_before)**(2.0_8)
       !             u_spell_young_crossterm=u_spell_young_crossterm+(u_spell_before)*(REAL(u_spell_now))
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_now_ave=u_spell_prime_now_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_ave=u_spell_prime_before_ave+(u_spell_before)
       !             u_spell_prime_now_sq=u_spell_prime_now_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_sq=u_spell_prime_before_sq+(u_spell_before)**(2.0_8)
       !             u_spell_prime_crossterm=u_spell_prime_crossterm+(u_spell_before)*(REAL(u_spell_now))
       !         END IF
       !
       !
       ! ! for previous spells with occ mobility
       ! IF (occmob_ind(t-4)==1) THEN
       !         u_spell_now_am_ave=u_spell_now_am_ave+(REAL(u_spell_now))
       !         u_spell_before_am_ave=u_spell_before_am_ave+(u_spell_before)
       !         u_spell_now_am_sq=u_spell_now_am_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_am_sq=u_spell_before_am_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_am=u_spell_crossterm_am+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_crossterm_am=u_spell_young_crossterm_am+(u_spell_before)*(REAL(u_spell_now))
       !             u_spell_young_now_am_ave=u_spell_now_am_ave+(REAL(u_spell_now))
       !             u_spell_young_before_am_ave=u_spell_young_before_am_ave+(u_spell_before)
       !             u_spell_young_now_am_sq=u_spell_now_am_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_am_sq=u_spell_young_before_am_sq+(u_spell_before)**(2.0_8)
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_crossterm_am=u_spell_prime_crossterm_am+(u_spell_before)*(REAL(u_spell_now))
       !             u_spell_prime_now_am_ave=u_spell_prime_now_am_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_am_ave=u_spell_prime_before_am_ave+(u_spell_before)
       !             u_spell_prime_now_am_sq=u_spell_prime_now_am_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_am_sq=u_spell_prime_before_am_sq+(u_spell_before)**(2.0_8)
       !         END IF
       ! END IF
       !
       ! ! for previous spells without occ mobility
       ! IF (occmob_ind(t-4)==2) THEN
       !         u_spell_now_as_ave=u_spell_now_as_ave+(REAL(u_spell_now))
       !         u_spell_before_as_ave=u_spell_before_as_ave+(u_spell_before)
       !         u_spell_now_as_sq=u_spell_now_as_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_as_sq=u_spell_before_as_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_as=u_spell_crossterm_as+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_crossterm_as=u_spell_young_crossterm_as+(u_spell_before)*(REAL(u_spell_now))
       !             u_spell_young_now_as_ave=u_spell_now_as_ave+(REAL(u_spell_now))
       !             u_spell_young_before_as_ave=u_spell_young_before_as_ave+(u_spell_before)
       !             u_spell_young_now_as_sq=u_spell_now_as_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_as_sq=u_spell_young_before_as_sq+(u_spell_before)**(2.0_8)
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_crossterm_as=u_spell_prime_crossterm_as+(u_spell_before)*(REAL(u_spell_now))
       !             u_spell_prime_now_as_ave=u_spell_prime_now_as_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_as_ave=u_spell_prime_before_as_ave+(u_spell_before)
       !             u_spell_prime_now_as_sq=u_spell_prime_now_as_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_as_sq=u_spell_prime_before_as_sq+(u_spell_before)**(2.0_8)
       !         END IF
       ! END IF
       !
       ! !------ repeat mobility
       !
       !
       ! ! for occ mob: MAM
       ! IF (occmob_ind(t-4)==1 .AND. occmob_ind(tcnt)==1 .AND. flag3==1) THEN
       !         u_spell_now_mam_ave=u_spell_now_mam_ave+(REAL(u_spell_now))
       !         u_spell_before_mam_ave=u_spell_before_mam_ave+(u_spell_before)
       !         u_spell_now_mam_sq=u_spell_now_mam_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_mam_sq=u_spell_before_mam_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_mam=u_spell_crossterm_mam+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_now_mam_ave=u_spell_now_mam_ave+(REAL(u_spell_now))
       !             u_spell_young_before_mam_ave=u_spell_young_before_mam_ave+(u_spell_before)
       !             u_spell_young_now_mam_sq=u_spell_now_mam_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_mam_sq=u_spell_young_before_mam_sq+(u_spell_before)**(2.0_8)
       !             u_spell_young_crossterm_mam=u_spell_young_crossterm_mam+(u_spell_before)*(REAL(u_spell_now))
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_now_mam_ave=u_spell_prime_now_mam_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_mam_ave=u_spell_prime_before_mam_ave+(u_spell_before)
       !             u_spell_prime_now_mam_sq=u_spell_prime_now_mam_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_mam_sq=u_spell_prime_before_mam_sq+(u_spell_before)**(2.0_8)
       !             u_spell_prime_crossterm_mam=u_spell_prime_crossterm_mam+(u_spell_before)*(REAL(u_spell_now))
       !         END IF
       ! END IF
       !
       ! ! for occ mob: SAM
       ! IF (occmob_ind(t-4)==1 .AND. occmob_ind(tcnt)==2 .AND. flag3==1) THEN
       !         u_spell_now_sam_ave=u_spell_now_sam_ave+(REAL(u_spell_now))
       !         u_spell_before_sam_ave=u_spell_before_sam_ave+(u_spell_before)
       !         u_spell_now_sam_sq=u_spell_now_sam_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_sam_sq=u_spell_before_sam_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_sam=u_spell_crossterm_sam+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_now_sam_ave=u_spell_now_sam_ave+(REAL(u_spell_now))
       !             u_spell_young_before_sam_ave=u_spell_young_before_sam_ave+(u_spell_before)
       !             u_spell_young_now_sam_sq=u_spell_now_sam_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_sam_sq=u_spell_young_before_sam_sq+(u_spell_before)**(2.0_8)
       !             u_spell_young_crossterm_sam=u_spell_young_crossterm_sam+(u_spell_before)*(REAL(u_spell_now))
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_now_sam_ave=u_spell_prime_now_sam_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_sam_ave=u_spell_prime_before_sam_ave+(u_spell_before)
       !             u_spell_prime_now_sam_sq=u_spell_prime_now_sam_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_sam_sq=u_spell_prime_before_sam_sq+(u_spell_before)**(2.0_8)
       !             u_spell_prime_crossterm_sam=u_spell_prime_crossterm_sam+(u_spell_before)*(REAL(u_spell_now))
       !         END IF
       ! END IF
       !
       ! ! for occ mob: MAS
       ! IF (occmob_ind(t-4)==2 .AND. occmob_ind(tcnt)==1 .AND. flag3==1) THEN
       !         u_spell_now_mas_ave=u_spell_now_mas_ave+(REAL(u_spell_now))
       !         u_spell_before_mas_ave=u_spell_before_mas_ave+(u_spell_before)
       !         u_spell_now_mas_sq=u_spell_now_mas_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_mas_sq=u_spell_before_mas_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_mas=u_spell_crossterm_mas+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_now_mas_ave=u_spell_now_mas_ave+(REAL(u_spell_now))
       !             u_spell_young_before_mas_ave=u_spell_young_before_mas_ave+(u_spell_before)
       !             u_spell_young_now_mas_sq=u_spell_now_mas_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_mas_sq=u_spell_young_before_mas_sq+(u_spell_before)**(2.0_8)
       !             u_spell_young_crossterm_mas=u_spell_young_crossterm_mas+(u_spell_before)*(REAL(u_spell_now))
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_now_mas_ave=u_spell_prime_now_mas_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_mas_ave=u_spell_prime_before_mas_ave+(u_spell_before)
       !             u_spell_prime_now_mas_sq=u_spell_prime_now_mas_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_mas_sq=u_spell_prime_before_mas_sq+(u_spell_before)**(2.0_8)
       !             u_spell_prime_crossterm_mas=u_spell_prime_crossterm_mas+(u_spell_before)*(REAL(u_spell_now))
       !         END IF
       ! END IF
       !
       ! ! for occ mob: SAS
       ! IF (occmob_ind(t-4)==2 .AND. occmob_ind(tcnt)==2 .AND. flag3==1) THEN
       !         u_spell_now_sas_ave=u_spell_now_sas_ave+(REAL(u_spell_now))
       !         u_spell_before_sas_ave=u_spell_before_sas_ave+(u_spell_before)
       !         u_spell_now_sas_sq=u_spell_now_sas_sq+(REAL(u_spell_now))**2.0_8
       !         u_spell_before_sas_sq=u_spell_before_sas_sq+(u_spell_before)**(2.0_8)
       !         u_spell_crossterm_sas=u_spell_crossterm_sas+(u_spell_before)*(REAL(u_spell_now))
       !         IF(age_sim(t)<youngcutoff) THEN
       !             u_spell_young_now_sas_ave=u_spell_now_sas_ave+(REAL(u_spell_now))
       !             u_spell_young_before_sas_ave=u_spell_young_before_sas_ave+(u_spell_before)
       !             u_spell_young_now_sas_sq=u_spell_now_sas_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_young_before_sas_sq=u_spell_young_before_sas_sq+(u_spell_before)**(2.0_8)
       !             u_spell_young_crossterm_sas=u_spell_young_crossterm_sas+(u_spell_before)*(REAL(u_spell_now))
       !         ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
       !             u_spell_prime_now_sas_ave=u_spell_prime_now_sas_ave+(REAL(u_spell_now))
       !             u_spell_prime_before_sas_ave=u_spell_prime_before_sas_ave+(u_spell_before)
       !             u_spell_prime_now_sas_sq=u_spell_prime_now_sas_sq+(REAL(u_spell_now))**2.0_8
       !             u_spell_prime_before_sas_sq=u_spell_prime_before_sas_sq+(u_spell_before)**(2.0_8)
       !             u_spell_prime_crossterm_sas=u_spell_prime_crossterm_sas+(u_spell_before)*(REAL(u_spell_now))
       !         END IF
       ! END IF
       !
       !
       !
       !


                !=============
                ! determine the outflow patterns of the second spell: Pmam, Pmas, Psam, Psas, Cmam, Csas etc.




    rep_mob_1_if:  IF(flag3==1) THEN           ! WE HAVE TWO COMPLETED SPELLS then we are in business


        !! ======= CORRECTED REPEAT MOBILITY ========================
        IF (corr_occmob_ind(t-4)==1 .AND.  corr_occmob_ind(tcnt)==1 .AND. flag3==1) THEN
                tot_corr_occafterocc=tot_corr_occafterocc+1.0_8
                IF (age_sim(t)<youngcutoff) THEN
                    tot_corr_occafterocc_y=tot_corr_occafterocc_y+1.0_8
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    tot_corr_occafterocc_p=tot_corr_occafterocc_p+1.0_8
                END IF
        ELSE IF (corr_occmob_ind(t-4)==1 .AND.  corr_occmob_ind(tcnt)==2 .AND. flag3==1) THEN
                tot_corr_noccafterocc=tot_corr_noccafterocc+1.0_8
                IF (age_sim(t)<youngcutoff) THEN
                    tot_corr_noccafterocc_y=tot_corr_noccafterocc_y+1.0_8
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    tot_corr_noccafterocc_p=tot_corr_noccafterocc_p+1.0_8
                END IF
        ELSE IF (corr_occmob_ind(t-4)==2 .AND.  corr_occmob_ind(tcnt)==1 .AND. flag3==1) THEN
                tot_corr_occafternocc=tot_corr_occafternocc+1.0_8
                IF (age_sim(t)<youngcutoff) THEN
                    tot_corr_occafternocc_y=tot_corr_occafternocc_y+1.0_8
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    tot_corr_occafternocc_p=tot_corr_occafternocc_p+1.0_8
                END IF
        ELSE IF (corr_occmob_ind(t-4)==2 .AND.  corr_occmob_ind(tcnt)==2 .AND. flag3==1) THEN
                tot_corr_noccafternocc=tot_corr_noccafternocc+1.0_8
                IF (age_sim(t)<youngcutoff) THEN
                    tot_corr_noccafternocc_y=tot_corr_noccafternocc_y+1.0_8
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    tot_corr_noccafternocc_p=tot_corr_noccafternocc_p+1.0_8
                END IF
        END IF
        
        !! ========MAM: MOVE AFTER MOVE================
        IF (occmob_ind(t-4)==1 .AND.  occmob_ind(tcnt)==1 .AND. flag3==1) THEN

            
                tot_occafterocc_vector(1)=tot_occafterocc_vector(1)+1.0_8
                
                !IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafterocc=tot_occafterocc+1.0_8
                
                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                        tot_occafterocc_vector(2)=tot_occafterocc_vector(2)+1.0_8
                        IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                            tot_occafterocc_vector(3)=tot_occafterocc_vector(3)+1.0_8
                            IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                tot_occafterocc_vector(4)=tot_occafterocc_vector(4)+1.0_8
                            END IF
                        END IF
                END IF

                IF(uspellength_ind>0 .AND. uspellength_ind<3) occafterocc_uspell(uspellength_ind)=occafterocc_uspell(uspellength_ind)+1.0_8

                stock_occafterocc=stock_occafterocc+REAL((tcnt2-tcnt)/4)
                IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafterocc_uspell(uspellength_ind)=stock_occafterocc_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)

                poccafterocc=poccafterocc+1.0_8
                IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafterocc_uspell(uspellength_ind)=poccafterocc_uspell(uspellength_ind)+1.0_8

                !! YOUNG
                IF (age_sim(t)<youngcutoff) THEN



                     tot_occafterocc_vector_young(1)=tot_occafterocc_vector_young(1)+1.0_8
                     IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                             tot_occafterocc_vector_young(2)=tot_occafterocc_vector_young(2)+1.0_8
                             IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                                 tot_occafterocc_vector_young(3)=tot_occafterocc_vector_young(3)+1.0_8
                                 IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                     tot_occafterocc_vector_young(4)=tot_occafterocc_vector_young(4)+1.0_8
                                 END IF
                             END IF
                    END IF

                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafterocc_y=tot_occafterocc_y+1.0_8
                    stock_occafterocc_y=stock_occafterocc_y+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafterocc_y_uspell(uspellength_ind)=stock_occafterocc_y_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    poccafterocc_y=poccafterocc_y+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafterocc_y_uspell(uspellength_ind)=poccafterocc_y_uspell(uspellength_ind)+1.0_8

                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN

                    tot_occafterocc_vector_prime(1)=tot_occafterocc_vector_prime(1)+1.0_8
                     IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                             tot_occafterocc_vector_prime(2)=tot_occafterocc_vector_prime(2)+1.0_8
                             IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                                 tot_occafterocc_vector_prime(3)=tot_occafterocc_vector_prime(3)+1.0_8
                                 IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                     tot_occafterocc_vector_prime(4)=tot_occafterocc_vector_prime(4)+1.0_8
                                 END IF
                             END IF
                    END IF


                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafterocc_p=tot_occafterocc_p+1.0_8
                    stock_occafterocc_p=stock_occafterocc_p+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafterocc_p_uspell(uspellength_ind)=stock_occafterocc_p_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    poccafterocc_p=poccafterocc_p+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafterocc_p_uspell(uspellength_ind)=poccafterocc_p_uspell(uspellength_ind)+1.0_8
                END IF

                
        !!  ==============SAM -- NOCC AFTER OCC============================= 
        ELSE IF(occmob_ind(t-4)==1 .AND. occmob_ind(tcnt)==2 .AND. flag3==1) THEN              ! FOUND BEGINNING subsequent spell without occ change

                !IF(unempdur_estatus((tcnt+4)/4)==0) tot_noccafterocc=tot_noccafterocc+1.0_8
                !IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafterocc=tot_noccafterocc+1.0_8
                
                tot_noccafterocc_vector(1)=tot_noccafterocc_vector(1)+1.0_8

                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                    tot_noccafterocc_vector(2)=tot_noccafterocc_vector(2)+1.0_8
                    IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_noccafterocc_vector(3)=tot_noccafterocc_vector(3)+1.0_8
                        IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                            tot_noccafterocc_vector(4)=tot_noccafterocc_vector(4)+1.0_8
                        END IF
                    END IF
                END IF

                IF (uspellength_ind>0) noccafterocc_uspell(uspellength_ind)=noccafterocc_uspell(uspellength_ind)+1.0_8
                stock_noccafterocc=stock_noccafterocc+REAL((tcnt2-tcnt)/4)
                IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafterocc_uspell(uspellength_ind)=stock_noccafterocc_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                pnoccafterocc=pnoccafterocc+1.0_8
                IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafterocc_uspell(uspellength_ind)=pnoccafterocc_uspell(uspellength_ind)+1.0_8

                
                !! YOUNG NOCC AFTER OCC
                IF (age_sim(t)<youngcutoff) THEN

                tot_noccafterocc_vector_young(1)=tot_noccafterocc_vector_young(1)+1.0_8

                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                    tot_noccafterocc_vector_young(2)=tot_noccafterocc_vector_young(2)+1.0_8
                    IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_noccafterocc_vector_young(3)=tot_noccafterocc_vector_young(3)+1.0_8
                        IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                            tot_noccafterocc_vector_young(4)=tot_noccafterocc_vector_young(4)+1.0_8
                        END IF
                    END IF
                END IF


                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafterocc_y=tot_noccafterocc_y+1.0_8

                    stock_noccafterocc_y=stock_noccafterocc_y+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafterocc_y_uspell(uspellength_ind)=stock_noccafterocc_y_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    pnoccafterocc_y=pnoccafterocc_y+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafterocc_y_uspell(uspellength_ind)=pnoccafterocc_y_uspell(uspellength_ind)+1.0_8
                
                !! PRIME NOCC AFTER OCC
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN

                tot_noccafterocc_vector_prime(1)=tot_noccafterocc_vector_prime(1)+1.0_8

                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                    tot_noccafterocc_vector_prime(2)=tot_noccafterocc_vector_prime(2)+1.0_8
                    IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_noccafterocc_vector_prime(3)=tot_noccafterocc_vector_prime(3)+1.0_8
                        IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                            tot_noccafterocc_vector_prime(4)=tot_noccafterocc_vector_prime(4)+1.0_8
                        END IF
                    END IF
                END IF


                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafterocc_p=tot_noccafterocc_p+1.0_8
                    stock_noccafterocc_p=stock_noccafterocc_p+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafterocc_p_uspell(uspellength_ind)=stock_noccafterocc_p_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    pnoccafterocc_p=pnoccafterocc_p+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafterocc_p_uspell(uspellength_ind)=pnoccafterocc_p_uspell(uspellength_ind)+1.0_8
                END IF

        END IF

        !! MAS -- OCC AFTER NOCC
        IF (occmob_ind(tcnt)==1 .AND. occmob_ind(t-4)==2 .AND. flag3==1) THEN  ! MAS: OCC MOVE AFTER OCC STAY, i.e. OccafterNocc

                !IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafternocc=tot_occafternocc+1.0_8

                tot_occafternocc_vector(1)=tot_occafternocc_vector(1)+1.0_8
                
                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                    tot_occafternocc_vector(2)=tot_occafternocc_vector(2)+1.0_8
                    IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                    tot_occafternocc_vector(3)=tot_occafternocc_vector(3)+1.0_8
                        IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                            tot_occafternocc_vector(4)=tot_occafternocc_vector(4)+1.0_8
                        END IF
                    END IF
                END IF

                IF(uspellength_ind>0 .AND. uspellength_ind<3) occafternocc_uspell(uspellength_ind)=occafternocc_uspell(uspellength_ind)+1.0_8
                stock_occafternocc=stock_occafternocc+REAL((tcnt2-tcnt)/4)
                IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafternocc_uspell(uspellength_ind)=stock_occafternocc_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                poccafternocc=poccafternocc+1.0_8
                IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafternocc_uspell(uspellength_ind)=poccafternocc_uspell(uspellength_ind)+1.0_8

                !!-- AGE SPLIT MAS
                !! YOUNG
                IF (age_sim(t)<youngcutoff) THEN


                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafternocc_y=tot_occafternocc_y+1.0_8


                    tot_occafternocc_vector_young(1)=tot_occafternocc_vector_young(1)+1.0_8
                    IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                        tot_occafternocc_vector_young(2)=tot_occafternocc_vector_young(2)+1.0_8
                        IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_occafternocc_vector_young(3)=tot_occafternocc_vector_young(3)+1.0_8
                            IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                tot_occafternocc_vector_young(4)=tot_occafternocc_vector_young(4)+1.0_8
                            END IF
                        END IF
                    END IF
                    
                    
                    
                    stock_occafternocc_y=stock_occafternocc_y+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafternocc_y_uspell(uspellength_ind)=stock_occafternocc_y_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    poccafternocc_y=poccafternocc_y+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafternocc_y_uspell(uspellength_ind)=poccafternocc_y_uspell(uspellength_ind)+1.0_8
                
                !! PRIME-AGED
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN

                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_occafternocc_p=tot_occafternocc_p+1.0_8

                    tot_occafternocc_vector_prime(1)=tot_occafternocc_vector_prime(1)+1.0_8
                    IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                        tot_occafternocc_vector_prime(2)=tot_occafternocc_vector_prime(2)+1.0_8
                        IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_occafternocc_vector_prime(3)=tot_occafternocc_vector_prime(3)+1.0_8
                            IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                tot_occafternocc_vector_prime(4)=tot_occafternocc_vector_prime(4)+1.0_8
                            END IF
                        END IF
                    END IF
                    
                    stock_occafternocc_p=stock_occafternocc_p+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_occafternocc_p_uspell(uspellength_ind)=stock_occafternocc_p_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    poccafternocc_p=poccafternocc_p+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) poccafternocc_p_uspell(uspellength_ind)=poccafternocc_p_uspell(uspellength_ind)+1.0_8
                END IF
                !!-- AGE SPLIT MAS
                
        !! SAS                
        ELSE IF(occmob_ind(tcnt)==2 .AND. occmob_ind(t-4)==2 .AND. flag3==1) THEN           ! SAS: occupational stay after occupational stay

                !IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafternocc=tot_noccafternocc+1.0_8

                tot_noccafternocc_vector(1)=tot_noccafternocc_vector(1)+1.0_8
                IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                    tot_noccafternocc_vector(2)=tot_noccafternocc_vector(2)+1.0_8
                    IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                        tot_noccafternocc_vector(3)=tot_noccafternocc_vector(3)+1.0_8
                        IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                            tot_noccafternocc_vector(4)=tot_noccafternocc_vector(4)+1.0_8
                        END IF
                    END IF
                END IF

                IF(uspellength_ind>0 .AND. uspellength_ind<3) noccafternocc_uspell(uspellength_ind)=noccafternocc_uspell(uspellength_ind)+1.0_8
                stock_noccafternocc=stock_noccafternocc+REAL((tcnt2-tcnt)/4)
                IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafternocc_uspell(uspellength_ind)=stock_noccafternocc_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                pnoccafternocc=pnoccafternocc+1.0_8
                IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafternocc_uspell(uspellength_ind)=pnoccafternocc_uspell(uspellength_ind)+1.0_8

                IF (age_sim(t)<youngcutoff) THEN
                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafternocc_y=tot_noccafternocc_y+1.0_8

                    tot_noccafternocc_vector_young(1)=tot_noccafternocc_vector_young(1)+1.0_8
                    IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                        tot_noccafternocc_vector_young(2)=tot_noccafternocc_vector_young(2)+1.0_8
                        IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                            tot_noccafternocc_vector_young(3)=tot_noccafternocc_vector_young(3)+1.0_8
                            IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                tot_noccafternocc_vector_young(4)=tot_noccafternocc_vector_young(4)+1.0_8
                            END IF
                        END IF
                    END IF


                    stock_noccafternocc_y=stock_noccafternocc_y+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafternocc_y_uspell(uspellength_ind)=stock_noccafternocc_y_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    pnoccafternocc_y=pnoccafternocc_y+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafternocc_y_uspell(uspellength_ind)=pnoccafternocc_y_uspell(uspellength_ind)+1.0_8
                
                !! PRIME
                ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
                    IF(unempdur_estatus((tcnt+4)/4)>0) tot_noccafternocc_p=tot_noccafternocc_p+1.0_8

                    tot_noccafternocc_vector_prime(1)=tot_noccafternocc_vector_prime(1)+1.0_8
                    IF(unempdur_estatus((tcnt+4)/4)>0) THEN
                        tot_noccafternocc_vector_prime(2)=tot_noccafternocc_vector_prime(2)+1.0_8
                        IF(unempdur_estatus((tcnt+8)/4)>0) THEN
                            tot_noccafternocc_vector_prime(3)=tot_noccafternocc_vector_prime(3)+1.0_8
                            IF(unempdur_estatus((tcnt+12)/4)>0) THEN
                                tot_noccafternocc_vector_prime(4)=tot_noccafternocc_vector_prime(4)+1.0_8
                            END IF
                        END IF
                    END IF


                    stock_noccafternocc_p=stock_noccafternocc_p+REAL((tcnt2-tcnt)/4)
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) stock_noccafternocc_p_uspell(uspellength_ind)=stock_noccafternocc_p_uspell(uspellength_ind)+REAL((tcnt2-tcnt)/4)
                    pnoccafternocc_p=pnoccafternocc_p+1.0_8
                    IF(uspellength_ind>0 .AND. uspellength_ind<3) pnoccafternocc_p_uspell(uspellength_ind)=pnoccafternocc_p_uspell(uspellength_ind)+1.0_8
                END IF

        END IF


    END IF rep_mob_1_if
    END IF repeat_u_entry_1_if

    IF (flag3==1 .OR. flag3==2) THEN
        flag3=0
        EXIT forward_loop2
    END IF

    END DO forward_loop2        ! looking for the second entry into unemployment


    
END IF first_u_spell_ends_if
END IF flag_windowsurvival_if
    
END DO inside_pseudo_sipp_window_do ![t]

    
    !! outside the inner sipp window, count the spells according to definition
    !! ALL WORKERS
    IF(tempcounter>1) THEN
    DO tcnt=1, tempcounter-1
        IF (repeatmobspells_matrix(tcnt,1)>0) THEN        !! INITIAL USPELL = OCC MOVE
            IF(repeatmobspells_matrix(tcnt+1,1)>1) THEN       !! SUBSEQUENT USPELL = ALSO OCC MOVE; BUT NEED TWO MONTHS IN U
                tot_occafterocc=tot_occafterocc+1.0
            ELSEIF(repeatmobspells_matrix(tcnt+1,2)>1) THEN   !! SUBSEQUENT USPELL = NOCC STAY; BUT NEED TWO MONTHS IN U
                tot_noccafterocc=tot_noccafterocc+1.0
            END IF 
        ELSEIF (repeatmobspells_matrix(tcnt,2)>0) THEN        !! INITIAL USPELL = NOCC STAY
            IF(repeatmobspells_matrix(tcnt+1,1)>1) THEN       !! SUBSEQUENT USPELL = ALSO OCC MOVE; BUT NEED TWO MONTHS IN U
                tot_occafternocc=tot_occafternocc+1.0
            ELSEIF(repeatmobspells_matrix(tcnt+1,2)>1) THEN   !! SUBSEQUENT USPELL = NOCC STAY; BUT NEED TWO MONTHS IN U
                tot_noccafternocc=tot_noccafternocc+1.0
            END IF
        END IF 
        
    !! YOUNG
    !! PRIME-AGED
    END DO ![tcnt]
    END IF 
    
    
    
    
!*************************
! EXIT WINDOW (MOVE WINDOW BY TEMPINT (192 DEFAULT)
!*************************
!END IF
counter1=counter1+tempint+1
END DO sippwindow_do






!!!------------------------------------------------
!!! RETURNS TO TENURE
!!!------------------------------------------------

        ! returns to tenure
        DO t=tmin_sim2+1, tmax_sim2
            IF(hcten_sim(t)==240 .AND. e_sim(t)==1) THEN
                tenure5yr_do: DO counter1=t-239,MAX(1,t-288),-1
                    IF(e_sim(counter1)==1 .AND. wage_sim(counter1)>0.0_8 .AND. wage_sim(t)>0.0_8) THEN
                        returnstenure5y=log(wage_sim(t))-log(wage_sim(counter1))+returnstenure5y
                        returnstenure5ycounter=returnstenure5ycounter+1
                        EXIT tenure5yr_do
                    END IF
                END DO tenure5yr_do
            END IF
        END DO

        DO t=tmin_sim2+1, tmax_sim2
            IF(hcten_sim(t)==480 .AND. e_sim(t)==1) THEN
                tenure10yr_do: DO counter1=t-479,MAX(1,t-528),-1
                    IF(e_sim(counter1)==1 .AND. wage_sim(counter1)>0.0_8 .AND. wage_sim(t)>0.0_8) THEN
                        returnstenure10y=log(wage_sim(t))-log(wage_sim(counter1))+returnstenure10y
                        returnstenure10ycounter=returnstenure10ycounter+1
                        EXIT tenure10yr_do
                    END IF
                END DO tenure10yr_do
            END IF
    END DO






!!!------------------------------------------------
!!! UNEMPLOYMENT DURATIONS
!!!------------------------------------------------

! DO t=tmin_sim2+1, tmax_sim2, 4
!
!     !! WITH TIME AGGREGATION CORRECTION
!     ! entry into unemployment
!     IF (duration_sim(t)>=0 .AND. duration_sim(t)<4    ) duration2(0)=duration2(0)+1.0_8
!     ! seen unemployed 1 months later
!     IF (duration_sim(t)>=4 .AND. duration_sim(t)<8    ) duration2(1)=duration2(1)+1.0_8
!     ! 4 months unemployed
!     IF (duration_sim(t)>=16 .AND. duration_sim(t)<20    ) duration2(2)=duration2(2)+1.0_8
!     ! 8 months unemployed, 12, 16, 20, 24 months unemployed
!     IF (duration_sim(t)>=32 .AND. duration_sim(t)<36    ) duration2(3)=duration2(3)+1.0_8
!     IF (duration_sim(t)>=48 .AND. duration_sim(t)<52    ) duration2(4)=duration2(4)+1.0_8
!     IF (duration_sim(t)>=64 .AND. duration_sim(t)<68    ) duration2(5)=duration2(5)+1.0_8
!     IF (duration_sim(t)>=80 .AND. duration_sim(t)<84    ) duration2(6)=duration2(6)+1.0_8
!     IF (duration_sim(t)>=96 .AND. duration_sim(t)<100    )duration2(7)=duration2(7)+1.0_8
!
!     ! young workers with time aggregation
!     IF(age_sim(t)<youngcutoff) THEN
!             IF (duration_sim(t)>=0 .AND. duration_sim(t)<4    ) duration2_young(0)=duration2_young(0)+1.0_8
!             IF (duration_sim(t)>=4 .AND. duration_sim(t)<8    )duration2_young(1)=duration2_young(1)+1.0_8
!             ! seen unemployed 4 months later
!             IF (duration_sim(t)>=16 .AND. duration_sim(t)<20    ) duration2_young(2)=duration2_young(2)+1.0_8
!             IF (duration_sim(t)>=32 .AND. duration_sim(t)<36    ) duration2_young(3)=duration2_young(3)+1.0_8
!             IF (duration_sim(t)>=48 .AND. duration_sim(t)<52    ) duration2_young(4)=duration2_young(4)+1.0_8
!             IF (duration_sim(t)>=64 .AND. duration_sim(t)<68    ) duration2_young(5)=duration2_young(5)+1.0_8
!             IF (duration_sim(t)>=80 .AND. duration_sim(t)<84    ) duration2_young(6)=duration2_young(6)+1.0_8
!             IF (duration_sim(t)>=96 .AND. duration_sim(t)<100    ) duration2_young(7)=duration2_young(7)+1.0_8
!
!     END IF
!
!     ! prime age workers with time aggregation
!
!     IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN
!         IF (duration_sim(t)>=0 .AND. duration_sim(t)<4    ) duration2_prime(0)=duration2_prime(0)+1.0_8
!         IF (duration_sim(t)>=4 .AND. duration_sim(t)<8    )  duration2_prime(1)=duration2_prime(1)+1.0_8
!         ! seen unemployed 4 months later
!         IF (duration_sim(t)>=16 .AND. duration_sim(t)<20    ) duration2_prime(2)=duration2_prime(2)+1.0_8
!         IF (duration_sim(t)>=32 .AND. duration_sim(t)<36    ) duration2_prime(3)=duration2_prime(3)+1.0_8
!         IF (duration_sim(t)>=48 .AND. duration_sim(t)<52    ) duration2_prime(4)=duration2_prime(4)+1.0_8
!         IF (duration_sim(t)>=64 .AND. duration_sim(t)<68    ) duration2_prime(5)=duration2_prime(5)+1.0_8
!         IF (duration_sim(t)>=80 .AND. duration_sim(t)<84    ) duration2_prime(6)=duration2_prime(6)+1.0_8
!         IF (duration_sim(t)>=96 .AND. duration_sim(t)<100    ) duration2_prime(7)=duration2_prime(7)+1.0_8
!     END IF
!
!
!         IF(duration_sim(t)>0 .AND. duration_sim(t)<5) ult5wk=ult5wk+1.0_8
!         IF(duration_sim(t)>=5 .AND. duration_sim(t)<15) u5lt15wk=u5lt15wk+1.0_8
!         IF(duration_sim(t)>=15 .AND. duration_sim(t)<27) u15lt27wk=u15lt27wk+1.0_8
!         IF(duration_sim(t)>=27) ugt27wk=ugt27wk+1.0_8
!
! END DO
!

!!!===========================================================
!!!------------------------------------------------
!!! EMPLOYMENT DURATIONS AND SEPARATION RATES WITHIN THE FIRST YEAR OF HIRING &  SUBSEQUENT OCC MOB
!!!------------------------------------------------
!!!============================================================

! SEPARATIONS WITHIN THE FIRST 1-12 MONTHS OF AN EMPLOYED WORKER

oneyear_future_sep_do: &
DO t=MAX(tmin_sim2+8,8), tmax_sim2-52, 4
    IF (unempdur_estatus(t/4)==0 .AND. age_sim(t)>8) THEN
                    ! currently employed
    ! everybody who is at least one more year in sample
    IF(age_sim(t+48)==age_sim(t)+48) THEN
        sep_ave_m_counter=sep_ave_m_counter+1
        IF(unempdur_estatus((t+4)/4)>=1) THEN
            sep_ave_1m=sep_ave_1m+1.0_8
            sep_ave_2m=sep_ave_2m+1.0_8
            sep_ave_3m=sep_ave_3m+1.0_8
            sep_ave_4m=sep_ave_4m+1.0_8
            sep_ave_5m=sep_ave_5m+1.0_8
            sep_ave_6m=sep_ave_6m+1.0_8
            sep_ave_7m=sep_ave_7m+1.0_8
            sep_ave_8m=sep_ave_8m+1.0_8
            sep_ave_9m=sep_ave_9m+1.0_8
            sep_ave_10m=sep_ave_10m+1.0_8
            sep_ave_11m=sep_ave_11m+1.0_8
            sep_ave_12m=sep_ave_12m+1.0_8
        ELSE
            IF(unempdur_estatus((t+8)/4)>=1) THEN
            sep_ave_2m=sep_ave_2m+1.0_8
                sep_ave_3m=sep_ave_3m+1.0_8
                sep_ave_4m=sep_ave_4m+1.0_8
                sep_ave_5m=sep_ave_5m+1.0_8
                sep_ave_6m=sep_ave_6m+1.0_8
                sep_ave_7m=sep_ave_7m+1.0_8
                sep_ave_8m=sep_ave_8m+1.0_8
                sep_ave_9m=sep_ave_9m+1.0_8
                sep_ave_10m=sep_ave_10m+1.0_8
                sep_ave_11m=sep_ave_11m+1.0_8
                sep_ave_12m=sep_ave_12m+1.0_8
            ELSE
                IF(unempdur_estatus((t+12)/4)>=1) THEN
                    sep_ave_3m=sep_ave_3m+1.0_8
                    sep_ave_4m=sep_ave_4m+1.0_8
                    sep_ave_5m=sep_ave_5m+1.0_8
                    sep_ave_6m=sep_ave_6m+1.0_8
                    sep_ave_7m=sep_ave_7m+1.0_8
                    sep_ave_8m=sep_ave_8m+1.0_8
                    sep_ave_9m=sep_ave_9m+1.0_8
                    sep_ave_10m=sep_ave_10m+1.0_8
                    sep_ave_11m=sep_ave_11m+1.0_8
                    sep_ave_12m=sep_ave_12m+1.0_8
                ELSE
                    IF(unempdur_estatus((t+16)/4)>=1) THEN
                            sep_ave_4m=sep_ave_4m+1.0_8
                            sep_ave_5m=sep_ave_5m+1.0_8
                            sep_ave_6m=sep_ave_6m+1.0_8
                            sep_ave_7m=sep_ave_7m+1.0_8
                            sep_ave_8m=sep_ave_8m+1.0_8
                            sep_ave_9m=sep_ave_9m+1.0_8
                            sep_ave_10m=sep_ave_10m+1.0_8
                            sep_ave_11m=sep_ave_11m+1.0_8
                            sep_ave_12m=sep_ave_12m+1.0_8
                        ELSE
                            IF(unempdur_estatus((t+20)/4)>=1) THEN
                                sep_ave_5m=sep_ave_5m+1.0_8
                                sep_ave_6m=sep_ave_6m+1.0_8
                                sep_ave_7m=sep_ave_7m+1.0_8
                                sep_ave_8m=sep_ave_8m+1.0_8
                                sep_ave_9m=sep_ave_9m+1.0_8
                                sep_ave_10m=sep_ave_10m+1.0_8
                                sep_ave_11m=sep_ave_11m+1.0_8
                                sep_ave_12m=sep_ave_12m+1.0_8
                            ELSE
                                IF(unempdur_estatus((t+24)/4)>=1) THEN
                                    sep_ave_6m=sep_ave_6m+1.0_8
                                    sep_ave_7m=sep_ave_7m+1.0_8
                                    sep_ave_8m=sep_ave_8m+1.0_8
                                    sep_ave_9m=sep_ave_9m+1.0_8
                                    sep_ave_10m=sep_ave_10m+1.0_8
                                    sep_ave_11m=sep_ave_11m+1.0_8
                                    sep_ave_12m=sep_ave_12m+1.0_8
                                ELSE
                                    IF(unempdur_estatus((t+28)/4)>=1) THEN
                                        sep_ave_7m=sep_ave_7m+1.0_8
                                        sep_ave_8m=sep_ave_8m+1.0_8
                                        sep_ave_9m=sep_ave_9m+1.0_8
                                        sep_ave_10m=sep_ave_10m+1.0_8
                                        sep_ave_11m=sep_ave_11m+1.0_8
                                        sep_ave_12m=sep_ave_12m+1.0_8
                                    ELSE
                                        IF(unempdur_estatus((t+32)/4)>=1) THEN
                                            sep_ave_8m=sep_ave_8m+1.0_8
                                            sep_ave_9m=sep_ave_9m+1.0_8
                                            sep_ave_10m=sep_ave_10m+1.0_8
                                            sep_ave_11m=sep_ave_11m+1.0_8
                                            sep_ave_12m=sep_ave_12m+1.0_8
                                        ELSE
                                            IF(unempdur_estatus((t+36)/4)>=1) THEN
                                                    sep_ave_9m=sep_ave_9m+1.0_8
                                                    sep_ave_10m=sep_ave_10m+1.0_8
                                                    sep_ave_11m=sep_ave_11m+1.0_8
                                                    sep_ave_12m=sep_ave_12m+1.0_8
                                                ELSE
                                                    IF(unempdur_estatus((t+40)/4)>=1) THEN
                                                        sep_ave_10m=sep_ave_10m+1.0_8
                                                        sep_ave_11m=sep_ave_11m+1.0_8
                                                        sep_ave_12m=sep_ave_12m+1.0_8
                                                    ELSE
                                                       IF(unempdur_estatus((t+44)/4)>=1) THEN
                                                                sep_ave_11m=sep_ave_11m+1.0_8
                                                                sep_ave_12m=sep_ave_12m+1.0_8
                                                        ELSE
                                                              IF(unempdur_estatus((t+48)/4)>=1) THEN
                                                                sep_ave_12m=sep_ave_12m+1.0_8
                                                                END IF
                                                        END IF

                                                    END IF
                                                END IF
                                        END IF
                                    END IF

                                END IF

                            END IF

                        END IF

                    END IF
                END IF
            END IF
        END IF
    END IF
    END DO oneyear_future_sep_do

!==================================
! SEPARATIONS ONE YEAR AFTER HIRING

    oneyear_future_sep_do_afterhire: &
DO t=MAX(tmin_sim2+8,8), tmax_sim2-52, 4
    IF (unempdur_estatus(t/4)==0 .AND. unempdur_estatus((t-4)/4)>=1 .AND. age_sim(t)>8) THEN
                    ! newly hired from recorded unemployment
    ! everybody who is at least one more year in sample
    IF(age_sim(t+48)==age_sim(t)+48) THEN
        sep_ave_m_posthire_counter=sep_ave_m_posthire_counter+1
        IF(unempdur_estatus((t+4)/4)>=1) THEN
            sep_ave_1m_posthire=sep_ave_1m_posthire+1.0_8
            sep_ave_2m_posthire=sep_ave_2m_posthire+1.0_8
            sep_ave_3m_posthire=sep_ave_3m_posthire+1.0_8
            sep_ave_4m_posthire=sep_ave_4m_posthire+1.0_8
            sep_ave_5m_posthire=sep_ave_5m_posthire+1.0_8
            sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
            sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
            sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
            sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
            sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
            sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
            sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
        ELSE
            IF(unempdur_estatus((t+8)/4)>=1) THEN
                sep_ave_2m_posthire=sep_ave_2m_posthire+1.0_8
                sep_ave_3m_posthire=sep_ave_3m_posthire+1.0_8
                sep_ave_4m_posthire=sep_ave_4m_posthire+1.0_8
                sep_ave_5m_posthire=sep_ave_5m_posthire+1.0_8
                sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
                sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
            ELSE
                IF(unempdur_estatus((t+12)/4)>=1) THEN
                    sep_ave_3m_posthire=sep_ave_3m_posthire+1.0_8
                    sep_ave_4m_posthire=sep_ave_4m_posthire+1.0_8
                    sep_ave_5m_posthire=sep_ave_5m_posthire+1.0_8
                    sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
                    sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                    sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                    sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                    sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                    sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                    sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                ELSE
                    IF(unempdur_estatus((t+16)/4)>=1) THEN
                        sep_ave_4m_posthire=sep_ave_4m_posthire+1.0_8
                            sep_ave_5m_posthire=sep_ave_5m_posthire+1.0_8
                            sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
                            sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                            sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                            sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                            sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                            sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                            sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                        ELSE
                            IF(unempdur_estatus((t+20)/4)>=1) THEN
                                sep_ave_5m_posthire=sep_ave_5m_posthire+1.0_8
                                sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
                                sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                                sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                                sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                                sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                            ELSE
                                IF(unempdur_estatus((t+24)/4)>=1) THEN
                                    sep_ave_6m_posthire=sep_ave_6m_posthire+1.0_8
                                    sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                                    sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                                    sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                                    sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                    sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                    sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                ELSE
                                    IF(unempdur_estatus((t+28)/4)>=1) THEN
                                        sep_ave_7m_posthire=sep_ave_7m_posthire+1.0_8
                                        sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                                        sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                                        sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                        sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                        sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                    ELSE
                                        IF(unempdur_estatus((t+32)/4)>=1) THEN
                                            sep_ave_8m_posthire=sep_ave_8m_posthire+1.0_8
                                            sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                                            sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                            sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                            sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                        ELSE
                                            IF(unempdur_estatus((t+36)/4)>=1) THEN
                                                    sep_ave_9m_posthire=sep_ave_9m_posthire+1.0_8
                                                    sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                                    sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                                    sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                                ELSE
                                                    IF(unempdur_estatus((t+40)/4)>=1) THEN
                                                        sep_ave_10m_posthire=sep_ave_10m_posthire+1.0_8
                                                        sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                                        sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                                    ELSE
                                                        IF(unempdur_estatus((t+44)/4)>=1) THEN
                                                                sep_ave_11m_posthire=sep_ave_11m_posthire+1.0_8
                                                                sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                                        ELSE
                                                                IF(unempdur_estatus((t+48)/4)>=1) THEN
                                                                sep_ave_12m_posthire=sep_ave_12m_posthire+1.0_8
                                                                END IF
                                                        END IF

                                                    END IF
                                                END IF
                                        END IF
                                    END IF

                                END IF

                            END IF

                        END IF

                    END IF
                END IF
            END IF
        END IF
    END IF
    END DO oneyear_future_sep_do_afterhire

!=========
! SEPARATIONS IN FIRST YEAR, SUBSEQUENT MOBILITY
!=========
DO t=MAX(tmin_sim2+1,2), tmax_sim2-8, 4

    !================
    ! employment recorded within first year
    IF ((e_sim(t)==1 .AND. e_duration_sim(t)<48) .OR.  & ! employed, in the first year of employment
            (e_sim(t-4)==1 .AND. e_duration_sim(t-4)<44 .AND. (e_sim(t-3)==1 .OR. e_sim(t-2)==1 .OR. e_sim(t-1)==1) )) THEN ! STILL COUNTED AS EMPLOYED                                     ! unemployed in the week of measurement, but not long enough to be counted
                    !(sets out eligible employment for the denominator)

        IF(occmob_ind(t)==3 .OR. (occmob_ind(t-4)==3 .AND. e_sim(t)==0) ) THEN                 ! occ change before

            ! BASELINE: EMPLOYMENT IN DENOMINATOR
            sepocc_ave_counter=sepocc_ave_counter+1.0_8
            IF(age_sim(t)<youngcutoff) sepocc_y_ave_counter=sepocc_y_ave_counter+1.0_8
            IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) sepocc_p_ave_counter=sepocc_p_ave_counter+1.0_8

            ! SEPARATION
            IF (e_sim(t+1)==0 .AND. e_sim(t+2)==0 .AND. e_sim(t+3)==0 .AND. e_sim(t+4)==0 )   THEN                  ! lost job within the month
                sepocc_ave=sepocc_ave+1.0_8
                IF(age_sim(t)<youngcutoff) sepocc_y_ave=sepocc_y_ave+1.0_8
                IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) sepocc_p_ave=sepocc_p_ave+1.0_8
            END IF



        ELSE IF (occmob_ind(t)==4 .OR. (occmob_ind(t-4)==4 .AND. e_sim(t)==0)) THEN


            sepnocc_ave_counter=sepnocc_ave_counter+1.0_8
            IF(age_sim(t)<youngcutoff) sepnocc_y_ave_counter=sepnocc_y_ave_counter+1.0_8
            IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) sepnocc_p_ave_counter=sepnocc_p_ave_counter+1.0_8

            ! SEPARATION
            IF (e_sim(t+1)==0 .AND. e_sim(t+2)==0 .AND. e_sim(t+3)==0 .AND. e_sim(t+4)==0 ) THEN
                sepnocc_ave=sepnocc_ave+1.0_8
                IF(age_sim(t)<youngcutoff) sepnocc_y_ave=sepnocc_y_ave+1.0_8
                IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) sepnocc_p_ave=sepnocc_p_ave+1.0_8
            END IF
        END IF
    END IF

    !========================
    ! employment durations

    IF(e_duration_sim(t)>0 .AND. (e_duration_sim(t)-(e_duration_sim(t)/16)*16)<4) THEN                      !! only in the 1st, 5th, 9th months etc.
        IF(t-e_duration_sim(t)>0.0_8) THEN              ! just to make sure that the indices do not cause a crash
            !weeks mapping into indicators in empduration 0: 1-16, 1:17-32, etc. 6:97-112, 7:113+
            IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)<=112)  empduration_nocc((e_duration_sim(t)-1)/16)=empduration_nocc((e_duration_sim(t)-1)/16)+1.0
            IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)>112)   empduration_nocc(7)=empduration_nocc(7)+1.0
            IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)<=112) empduration_occ((e_duration_sim(t)-1)/16)=empduration_occ((e_duration_sim(t)-1)/16)+1.0
            IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)>112)  empduration_occ(7)=empduration_nocc(7)+1.0

            ! employment hazards with age
            IF(age_sim(t)<youngcutoff) THEN    ! young guy
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)<=112)  empduration_nocc_y((e_duration_sim(t)-1)/16)=empduration_nocc_y((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)>112)   empduration_nocc_y(7)=empduration_nocc_y(7)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)<=112) empduration_occ_y((e_duration_sim(t)-1)/16)=empduration_occ_y((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)>112)  empduration_occ_y(7)=empduration_nocc_y(7)+1.0
            ELSE IF (age_sim(t)>lowcutoff .AND. age_sim(t)<highcutoff) THEN ! prime guy
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)<=112)  empduration_nocc_p((e_duration_sim(t)-1)/16)=empduration_nocc_p((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)>112)   empduration_nocc_p(7)=empduration_nocc_p(7)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)<=112) empduration_occ_p((e_duration_sim(t)-1)/16)=empduration_occ_p((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)>112)  empduration_occ_p(7)=empduration_nocc_p(7)+1.0
            END IF

            ! employment hazards with previous unemployment spell
            IF(duration_sim(t-e_duration_sim(t))>0 .AND. duration_sim(t-e_duration_sim(t))<4) THEN    ! unemployed for less than 4 months
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)<=112)  empduration_nocc_shuspell((e_duration_sim(t)-1)/16)=empduration_nocc_shuspell((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)>112)   empduration_nocc_shuspell(7)=empduration_nocc_shuspell(7)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)<=112) empduration_occ_shuspell((e_duration_sim(t)-1)/16)=empduration_occ_shuspell((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)>112)  empduration_occ_shuspell(7)=empduration_nocc_shuspell(7)+1.0
            ELSE IF(duration_sim(t-e_duration_sim(t))>=4 .AND. duration_sim(t-e_duration_sim(t))<8) THEN    ! unemployed for more than 4 months, less than 8
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)<=112)  empduration_nocc_luspell((e_duration_sim(t)-1)/16)=empduration_nocc_luspell((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))>0 .AND. e_duration_sim(t)>112)   empduration_nocc_luspell(7)=empduration_nocc_luspell(7)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)<=112) empduration_occ_luspell((e_duration_sim(t)-1)/16)=empduration_occ_luspell((e_duration_sim(t)-1)/16)+1.0
                IF(hcten_sim(t-e_duration_sim(t))==0 .AND. e_duration_sim(t)>112)  empduration_occ_luspell(7)=empduration_nocc_luspell(7)+1.0
            END IF
        END IF
    END IF
END DO






!!!------------------------------------------------
!!! WRITING DOWN THE SIMULATIONS
!!!------------------------------------------------


threadno_if2: IF (threadnumber==1) THEN
IF(workerhistory_sim==1) THEN
!
!
!
    DO t=tmin_sim2+1,MIN(tmax_sim2, tmin_sim2+4800)
!
        !WHEN ARE THINGS RECORDED
        flag1=0
        IF ((i==nsim_gen/2 .OR. i==nsim_gen .OR. i<4) .AND. counter2==1) flag1=1
        IF (t<tmin_sim2+36 .AND. counter2==1 .AND. i<MIN(300, nsim_gen)) flag1=1
!
        history_if1: IF(flag1==1) THEN
!
!
        IF ((t==tmin_sim2+1 .AND. i==1).OR. (i<4 .AND. occmob_ind(t)>0 .AND. occmob_ind(MAX(t-1,1))==0 )) THEN
            write(14, FMT='(3(A6,A1), A4, A1)', ADVANCE='NO') 't', ',' ,'i', ',', 'age', ';', 'cocc', ','
            write(14, FMT='(5(A4,A1), A4, A1, 3(A7, A1))', ADVANCE='NO') 'esim', '|', 'isim', '|', 'hctn', '|', 'hind', '|', 'udur', '|', &
            'wage', '|', 'd_u_mon', '|', 'd_v_mon', '|', 'prod', '|'
            write(14, FMT='(3(A7, A1))', ADVANCE='NO') 'd_jf_mn', '|', 'd_sep_m', '|', 'd_prod', '|'
            write(14, FMT='(1(A7, A1))', ADVANCE='NO') ' test  ', '|'
!
            write(14, FMT='(3(A7, A1))', ADVANCE='NO') ' cocc  ', '|', 'cocc y', '|', 'cocc p ','|'
            write(14, FMT='(3(A7, A1))', ADVANCE='NO') 'cinf ', '|', 'cinf y   ', '|', 'cinf p ','|'
            write(14, FMT='(2(A7, A1))', ADVANCE='NO') 'pocc ', '|', 'pnocc   ', '|'
            write(14, FMT='(3(A7, A1))', ADVANCE='NO') ' nocc  ', '|', 'nocc y', '|', 'nocc p ','|'
            write(14, FMT='(3(A7, A1))', ADVANCE='NO') 'ninf ', '|', 'ninf y   ', '|', 'ninf p ','|'
!
            write(14, FMT='(2(A7, A1))', ADVANCE='NO') 'u_young', '|', 'u_prime', '|'
            write(14, FMT='(2(A7, A1))', ADVANCE='NO') 'no. yng', '|', 'no.prim', '|'
            write(14, FMT='(A3)', ADVANCE='NO') '|-|'
            write(14, FMT='(2(A7, A1))', ADVANCE='NO') 'islprod', '|', 'low isl', '|'
            write(14, FMT='(6(A7, A1))', ADVANCE='NO') 'sepprob', '|', 'jfprob', '|', 'zprob', '|', 'd_u_mn', '|', 'd_v_mn', '|', 'd_re_mn', '|'
            write(14, FMT='(3(A7, A1))') 'jf', '|', 'poliDW', '|', 'poliR', '|'
        END IF
!
        IF (t==tmin_sim2+1) write(14,*) '===================', i, '======================='
        IF ((t-tmin_sim2)/160*160-(t-tmin_sim2)==0) write(14,*) '----------------------------------------------'
        write(14, FMT='(3(I6,A1), I4, A1)', ADVANCE='NO') t, ',' ,i, ',' , age_sim(t), ';', occmob_ind(t), ','
        write(14, FMT='(5(I4,A1), F4.2, A1, 3(F7.1, A1))', ADVANCE='NO') e_sim(t), '|', island_sim(t), '|', hcten_sim(t), '|', hcind_sim(t), '|', duration_sim(t), '|', &
            wage_sim(t), '|', data_u_qtr((t-tmin_sim2)/12+1), '|', data_v_qtr((t-tmin_sim2)/12+1), '|', prod(aggprod_ind(t), MAX(hcind_sim(t),1), island_sim(t)), '|'
        write(14, FMT='(3(F7.2, A1))', ADVANCE='NO') data_jf_qtr((t-tmin_sim2)/12+1), '|', data_sep_qtr((t-tmin_sim2)/12+1), '|', data_prod_qtr((t-tmin_sim2)/12+1),'|'
        !write(14, FMT='(1(F7.0, A1))', ADVANCE='NO') data_temp_qtr((t-tmin_sim2)/12+1),  '|'        ! test variable, e.g. for repeat mobility
        write(14, FMT='(3(F7.0, A1))', ADVANCE='NO') data_cocc_qtr((t-tmin_sim2)/12+1), '|', data_cocc_young_qtr((t-tmin_sim2)/12+1), '|', data_cocc_prime_qtr((t-tmin_sim2)/12+1),'|'
        write(14, FMT='(3(F7.0, A1))', ADVANCE='NO') data_cocc_inflow_qtr((t-tmin_sim2)/12+1),'|', data_cocc_young_inflow_qtr((t-tmin_sim2)/12+1), '|', data_cocc_prime_inflow_qtr((t-tmin_sim2)/12+1),'|'
        write(14, FMT='(2(F7.0, A1))', ADVANCE='NO') data_pocc_qtr((t-tmin_sim2)/12+1),'|', data_pnocc_qtr((t-tmin_sim2)/12+1), '|'
        write(14, FMT='(3(F7.0, A1))', ADVANCE='NO') data_nocc_qtr((t-tmin_sim2)/12+1), '|', data_nocc_young_qtr((t-tmin_sim2)/12+1), '|', data_nocc_prime_qtr((t-tmin_sim2)/12+1),'|'
        write(14, FMT='(3(F7.0, A1))', ADVANCE='NO') data_nocc_inflow_qtr((t-tmin_sim2)/12+1),'|', data_nocc_young_inflow_qtr((t-tmin_sim2)/12+1), '|', data_nocc_prime_inflow_qtr((t-tmin_sim2)/12+1),'|'
!
        write(14, FMT='(2(F7.0, A1))', ADVANCE='NO') data_u_young_qtr((t-tmin_sim2)/12+1), '|', data_u_prime_qtr((t-tmin_sim2)/12+1), '|'
        write(14, FMT='(2(F7.0, A1))', ADVANCE='NO') data_young_qtr((t-tmin_sim2)/12+1), '|', data_prime_qtr((t-tmin_sim2)/12+1), '|'
!
        write(14, FMT='(A3)', ADVANCE='NO') '|-|'
!
!        IF(restricted_search_ind==1) THEN
!        IF(max_chosen_islands==0) WRITE(*,*) 'ERROR: ***** max_chosen_islands: illegal value*******'
!
!        IF(max_chosen_islands==0) WRITE(59,*) 'ERROR: ***** max_chosen_islands: illegal value*******'
!
!        IF(best_islands(max_chosen_islands, t)>=1 .AND. best_islands(max_chosen_islands, t)<=maxislands) THEN
!            write(14, FMT='(2(F7.4, A1))', ADVANCE='NO') zvector(islandprod_ind(island_sim(t),t)), '|', zvector(islandprod_ind(best_islands(max_chosen_islands, t),t)), '|'
!        ELSE
!            WRITE(59,*) 'error 71: best_islands, line 4165, best_islands(max_chosen_islands, t)=', best_islands(max_chosen_islands, t)
!            WRITE(59,*) '     at max_chosen_islands=', max_chosen_islands, ' and t=', t
!        END IF
!        END IF

        write(14, FMT='(3(F7.4, A1))', ADVANCE='NO') sepprobs(t),'|', jprobs(t), '|', zprobs(t),'|'
        write(14, FMT='(A3)') '|-|'
!++++++++++++++
    END IF history_if1
    END DO
!
END IF
END IF threadno_if2 !(threadnumber==1)






    !!!-----------------------------------------------------
    !!!  LOOP FOR EVERY SIMULATED PERSON UNTIL THE nsim_gen, max_gen is reached
    !!!-----------------------------------------------------

!WRITE(*,*) 'i=', i
END DO nsim_do
!CLOSE(55)
END DO gen_do

 threadno_if3: IF (threadnumber==1) THEN
 IF(workerhistory_sim==1) THEN
     CLOSE(unit=14)
 END IF
    END IF threadno_if3

!!!***************************************************************************************************************************





!IF (threadnumber==1) THEN
!OPEN(unit=94, file='monthly_series_nonnormalized.txt', status='replace', form='formatted')
!DO t=1, tmax_sim2
!WRITE(94, FMT='(I3, A1, 3(F11.4, A1))', ADVANCE='NO') aggprod_ind(t), '|', data_prod_qtr(t), '|', data_u_qtr(t), '|', data_v_qtr(t), '|'
!WRITE(94, FMT='(I3, A1, 3(F11.4, A1))') aggprod_ind(t), '|', data_prod_qtr(t)/(number_agents(t)-data_u_qtr(t)), '|', data_u_qtr(t)/number_agents(t), '|', data_v_qtr(t)/number_agents(t), '|'
!END DO
!CLOSE(94)
!END IF !(threadnumber==1)
!
!IF (threadnumber==1) THEN
!OPEN(unit=81, file='island_series_nonnormalized.txt', status='replace', form='formatted')
!DO zcnt=1, zpts
!WRITE(81, FMT='(I3, A1, 6(A11, A1))', ADVANCE='no') zcnt, '|', ' island_e1 ', '|', ' island_e2 ', '|', ' island_e3 ', '|', ' island_u1 ','|', ' island_u3 ','|', ' island_u3 ','|'
!END DO

!counter1=1
!counter2=2000
!
!DO t=counter1, counter2
!DO zcnt=1, zpts
!WRITE(81, FMT='(I3, A1, 3(F11.4, A1))', ADVANCE='NO') zcnt, '|', island_employment(zcnt,1,t), '|', &
!island_employment(zcnt,2,t), '|', island_employment(zcnt,3,t), '|'
!WRITE(81, FMT='(3(F11.4, A1))', ADVANCE='NO')  island_unemployment(zcnt,1, t), '|',&
!island_unemployment(zcnt,2, t), '|',island_unemployment(zcnt,3, t), '|'
!!WRITE(81, FMT='(I3, A1, 3(F11.4, A1))', ADVANCE='NO') aggprod_ind(t), '|', data_prod_qtr(t)/(number_agents(t)-data_u_qtr(t)), '|', data_u_qtr(t)/number_agents(t), '|', data_v_qtr(t)/number_agents(t), '|'
!END DO
!WRITE(81,FMT='(A1)') '!'
!END DO
!
!IF(tmax_sim2>4000) THEN
!    counter1=tmax_sim2-2000
!    counter2=tmax_sim2
!END IF
!
!DO t=counter1, counter2
!DO zcnt=1, zpts
!WRITE(81, FMT='(I3, A1, 3(F11.4, A1))', ADVANCE='NO') zcnt, '|', island_employment(zcnt,1,t), '|', &
!island_employment(zcnt,2,t), '|', island_employment(zcnt,3,t), '|'
!WRITE(81, FMT='(3(F11.4, A1))', ADVANCE='NO')  island_unemployment(zcnt,1, t), '|',&
!island_unemployment(zcnt,2, t), '|',island_unemployment(zcnt,3, t), '|'
!!WRITE(81, FMT='(I3, A1, 3(F11.4, A1))', ADVANCE='NO') aggprod_ind(t), '|', data_prod_qtr(t)/(number_agents(t)-data_u_qtr(t)), '|', data_u_qtr(t)/number_agents(t), '|', data_v_qtr(t)/number_agents(t), '|'
!END DO
!WRITE(81,FMT='(A1)') '!'
!END DO
!
!CLOSE(81)
!END IF !(threadnumber==1)




!!!!========================================================================
!!! NORMALIZE SERIES, AND HP DETREND the appropriate time series
!!!=========================================================================



! U EARLIER EMPLOYMENT
        ! UNEMPLOYMENT RATE FOR THOSE WITH EARLIER EMPLOYMENT IN THE FIRST 4 WAVES, i.e. 16 months, OR IN THE SUBSEQUENT PERIOD UNTIL T.

        IF(earlier_e_counter>0) unemp_earlier_e_ave=unemp_earlier_e_ave/REAL(earlier_e_counter)
        IF(young_earlier_e_counter>0) u_earlier_e_young_ave=u_earlier_e_young_ave/REAL(young_earlier_e_counter)
        IF(prime_earlier_e_counter>0) u_earlier_e_prime_ave=u_earlier_e_prime_ave/REAL(prime_earlier_e_counter)



! U PROPORTION

IF(counter_u_window_all>0) uproportion_window=REAL(counter_u_window)/REAL(counter_u_window_all)
IF(counter_u_window_all_y>0) uproportion_window_y=REAL(counter_u_window_y)/REAL(counter_u_window_all_y)
IF(counter_u_window_all_p>0) uproportion_window_p=REAL(counter_u_window_p)/REAL(counter_u_window_all_p)

uoccproportion_window=REAL(counter_uocc_window)/REAL(counter_uocc_window_all)
unoccproportion_window=REAL(counter_unocc_window)/REAL(counter_unocc_window_all)


! SEPARATION PROFILE POST-HIRING
IF(sep_ave_m_counter>0) THEN
    sep_ave_1m=sep_ave_1m/REAL(sep_ave_m_counter)
    sep_ave_2m=sep_ave_2m/REAL(sep_ave_m_counter)
    sep_ave_3m=sep_ave_3m/REAL(sep_ave_m_counter)
    sep_ave_4m=sep_ave_4m/REAL(sep_ave_m_counter)
    sep_ave_5m=sep_ave_5m/REAL(sep_ave_m_counter)
    sep_ave_6m=sep_ave_6m/REAL(sep_ave_m_counter)
    sep_ave_7m=sep_ave_7m/REAL(sep_ave_m_counter)
    sep_ave_8m=sep_ave_8m/REAL(sep_ave_m_counter)
    sep_ave_9m=sep_ave_9m/REAL(sep_ave_m_counter)
    sep_ave_10m=sep_ave_10m/REAL(sep_ave_m_counter)
    sep_ave_11m=sep_ave_11m/REAL(sep_ave_m_counter)
    sep_ave_12m=sep_ave_12m/REAL(sep_ave_m_counter)
END IF

IF(sep_ave_m_posthire_counter>0) THEN
            sep_ave_1m_posthire=sep_ave_1m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_2m_posthire=sep_ave_2m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_3m_posthire=sep_ave_3m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_4m_posthire=sep_ave_4m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_5m_posthire=sep_ave_5m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_6m_posthire=sep_ave_6m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_7m_posthire=sep_ave_7m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_8m_posthire=sep_ave_8m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_9m_posthire=sep_ave_9m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_10m_posthire=sep_ave_10m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_11m_posthire=sep_ave_11m_posthire/REAL(sep_ave_m_posthire_counter)
            sep_ave_12m_posthire=sep_ave_12m_posthire/REAL(sep_ave_m_posthire_counter)
END IF

! WAGE DISPERSION

IF(logwage_all_counter>0) logwage_disp_all=((logwage_disp_all/REAL(logwage_all_counter))-(((logwage_ave_all)**2.0_8)/(REAL(logwage_all_counter)**2.0)))
IF(logwage_highoccten_counter>0) logwage_disp_highoccten=((logwage_disp_highoccten/(REAL(logwage_highoccten_counter)))-(((logwage_ave_highoccten)**2.0_8)/(REAL(logwage_highoccten_counter)**2.0_8)))
IF(logwage_uhires_counter>0) logwage_disp_uhires=((logwage_disp_uhires/REAL(logwage_uhires_counter))-(((logwage_ave_uhires)**2.0_8)/(REAL(logwage_uhires_counter)**2.0_8)))
IF(logwage_all_counter<=0) logwage_disp_all=0.0_8
IF(logwage_highoccten_counter<=0) logwage_disp_highoccten=0.0_8
IF(logwage_uhires_counter<=0) logwage_disp_uhires=0.0_8

logwage_disp_diff_all=logwage_disp_all-logwage_disp_uhires
logwage_disp_diff_highoccten=logwage_disp_highoccten-logwage_disp_uhires

! REPEAT SEPARATIONS
IF (sepocc_afternocc_2_5yr_counter>0) sepocc_afternocc_2_5yr=sepocc_afternocc_2_5yr/REAL(sepocc_afternocc_2_5yr_counter)
IF (sepocc_afternocc_2_5yr_counter>0) sepocc_afternocc_2_5yr_2m=sepocc_afternocc_2_5yr_2m/REAL(sepocc_afternocc_2_5yr_counter)
IF (sepocc_afternocc_2_5yr_counter==0 ) sepocc_afternocc_2_5yr=0.0_8
IF (sepnocc_afternocc_2_5yr_counter>0) sepnocc_afternocc_2_5yr=sepnocc_afternocc_2_5yr/REAL(sepnocc_afternocc_2_5yr_counter)
IF (sepnocc_afternocc_2_5yr_counter>0) sepnocc_afternocc_2_5yr_2m=sepnocc_afternocc_2_5yr_2m/REAL(sepnocc_afternocc_2_5yr_counter)
IF (sepnocc_afternocc_2_5yr_counter==0 ) sepnocc_afternocc_2_5yr=0.0_8
IF (sep_afternocc_2_5yr_counter>0) sep_afternocc_2_5yr=sep_afternocc_2_5yr/REAL(sep_afternocc_2_5yr_counter)
IF (sep_afternocc_2_5yr_counter==0 ) sep_afternocc_2_5yr=0.0_8


IF (returnstenure10ycounter>0) THEN
returnstenure10y=returnstenure10y/REAL(returnstenure10ycounter)
END IF

IF (returnstenure5ycounter>0) THEN
returnstenure5y=returnstenure5y/REAL(returnstenure5ycounter)
END IF


!! job finding and tightness
DO t=1,tmax_qtr
IF (data_u_qtr(t)>0.0_8 .AND. data_u_qtr(t)<nsim_gen*max_gen) THEN
    data_theta_qtr(t)=REAL(data_v_qtr(t))/REAL(data_u_qtr(t))
    data_jf_qtr(t)=REAL(data_jf_qtr(t))/REAL(data_u_qtr(t))
ELSE
    data_theta_qtr(t)=0.0_8
    data_jf_qtr(t)=0.0_8
END IF
END DO

!! cost of hiring
tot_costofhires=tot_costofhires/tot_hires

!! separation and productivity
DO t=1, tmax_qtr
IF (data_e_qtr(t)>0.0_8) THEN
    data_sep_qtr(t)=data_sep_qtr(t)/(data_e_qtr(t))
ELSE
    data_sep_qtr(t)=0.0_8
END IF

IF (3.0_8*REAL(number_agents(t))-data_u_qtr(t)>0.0_8) THEN
    data_prod_qtr(t)=data_prod_qtr(t)/(data_e_qtr(t))
    data_wage_qtr(t)=data_wage_qtr(t)/(data_e_qtr(t))
ELSE
    data_prod_qtr(t)=0.0_8
    data_wage_qtr(t)=0.0_8
END IF
END DO

!! normalized by number of people (nsim)
!DO t=1,tmax_qtr
!    data_v_qtr(t)=data_v_qtr(t)/(3.0_8*REAL(number_agents(t)))
!END DO


!! separation and productivity
DO t=1, tmax_qtr
IF (data_e_young_qtr(t)>0) THEN
    data_sep_y_qtr(t)=data_sep_y_qtr(t)/(data_e_young_qtr(t))
ELSE
    data_sep_y_qtr(t)=0.0_8
END IF
END DO

DO t=1, tmax_qtr
IF (data_e_prime_qtr(t)>0.0_8) THEN
    data_sep_p_qtr(t)=data_sep_p_qtr(t)/(data_e_prime_qtr(t))
ELSE
    data_sep_p_qtr(t)=0.0_8
END IF
END DO



! jf, u of the young and prime aged
DO t=1, tmax_qtr

IF(data_u_young_qtr(t)>0.0) THEN
    data_jf_young_qtr(t)=REAL(data_jf_young_qtr(t))/REAL(data_u_young_qtr(t))
    data_u_young_qtr(t)=data_u_young_qtr(t)/(data_u_young_qtr(t)+data_e_young_qtr(t))
ELSE
    data_jf_young_qtr(t)=0.0_8
    data_u_young_qtr(t)=0.0_8
END IF



IF(data_u_prime_qtr(t)>0.0) THEN
    data_jf_prime_qtr(t)=REAL(data_jf_prime_qtr(t))/REAL(data_u_prime_qtr(t))
    data_u_prime_qtr(t)=data_u_prime_qtr(t)/(data_e_prime_qtr(t)+data_u_prime_qtr(t))
ELSE
    data_jf_prime_qtr(t)=0.0_8
    data_u_prime_qtr(t)=0.0_8
END IF

END DO



! normalized by the number of occ/nocc categorizable unemployed



        !===============
        ! repeat mobility

        tempreal=(REAL(tot_noccafterocc)+REAL(tot_occafterocc))
        IF (tempreal>0.0_8) tot_occafterocc=REAL(tot_occafterocc)/tempreal
        IF (tempreal>0.0_8) tot_noccafterocc=REAL(tot_noccafterocc)/tempreal

        tempreal=(REAL(tot_noccafterocc_y)+REAL(tot_occafterocc_y))
        IF (tempreal>0.0_8) tot_occafterocc_y=REAL(tot_occafterocc_y)/tempreal
        IF (tempreal>0.0_8) tot_noccafterocc_y=REAL(tot_noccafterocc_y)/tempreal

        tempreal=(REAL(tot_noccafterocc_p)+REAL(tot_occafterocc_p))
        IF (tempreal>0.0_8) tot_occafterocc_p=REAL(tot_occafterocc_p)/tempreal
        IF (tempreal>0.0_8) tot_noccafterocc_p=REAL(tot_noccafterocc_p)/tempreal

        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafterocc_vector(zcnt))+REAL(tot_occafterocc_vector(zcnt)))
            IF (tempreal>0.0_8) tot_occafterocc_vector(zcnt)=REAL(tot_occafterocc_vector(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafterocc_vector(zcnt)=REAL(tot_noccafterocc_vector(zcnt))/tempreal
        END DO
        ! young
        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafterocc_vector_young(zcnt))+REAL(tot_occafterocc_vector_young(zcnt)))
            IF (tempreal>0.0_8) tot_occafterocc_vector_young(zcnt)=REAL(tot_occafterocc_vector_young(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafterocc_vector_young(zcnt)=REAL(tot_noccafterocc_vector_young(zcnt))/tempreal
        END DO
        ! prime
        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafterocc_vector_prime(zcnt))+REAL(tot_occafterocc_vector_prime(zcnt)))
            IF (tempreal>0.0_8) tot_occafterocc_vector_prime(zcnt)=REAL(tot_occafterocc_vector_prime(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafterocc_vector_prime(zcnt)=REAL(tot_noccafterocc_vector_prime(zcnt))/tempreal
        END DO

        tempreal=(REAL(tot_corr_noccafterocc)+REAL(tot_corr_occafterocc))
        IF (tempreal>0.0_8) tot_corr_occafterocc=REAL(tot_corr_occafterocc)/tempreal
        IF (tempreal>0.0_8) tot_corr_noccafterocc=REAL(tot_corr_noccafterocc)/tempreal

        tempreal=(REAL(tot_corr_noccafterocc_y)+REAL(tot_corr_occafterocc_y))
        IF (tempreal>0.0_8) tot_corr_occafterocc_y=REAL(tot_corr_occafterocc_y)/tempreal
        IF (tempreal>0.0_8) tot_corr_noccafterocc_y=REAL(tot_corr_noccafterocc_y)/tempreal

        tempreal=(REAL(tot_corr_noccafterocc_p)+REAL(tot_corr_occafterocc_p))
        IF (tempreal>0.0_8) tot_corr_occafterocc_p=REAL(tot_corr_occafterocc_p)/tempreal
        IF (tempreal>0.0_8) tot_corr_noccafterocc_p=REAL(tot_corr_noccafterocc_p)/tempreal

        !! AFTER OCC STAY 
        
        tempreal=(REAL(tot_noccafternocc)+REAL(tot_occafternocc))
        IF (tempreal>0.0_8) tot_noccafternocc=REAL(tot_noccafternocc)/tempreal
        IF (tempreal>0.0_8) tot_occafternocc=REAL(tot_occafternocc)/tempreal

        tempreal=(REAL(tot_noccafternocc_y)+REAL(tot_occafternocc_y))
        IF (tempreal>0.0_8) tot_noccafternocc_y=REAL(tot_noccafternocc_y)/tempreal
        IF (tempreal>0.0_8) tot_occafternocc_y=REAL(tot_occafternocc_y)/tempreal

        tempreal=(REAL(tot_noccafternocc_p)+REAL(tot_occafternocc_p))
        IF (tempreal>0.0_8) tot_noccafternocc_p=REAL(tot_noccafternocc_p)/tempreal
        IF (tempreal>0.0_8) tot_occafternocc_p=REAL(tot_occafternocc_p)/tempreal

        
        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafternocc_vector(zcnt))+REAL(tot_occafternocc_vector(zcnt)))
            IF (tempreal>0.0_8) tot_occafternocc_vector(zcnt)=REAL(tot_occafternocc_vector(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafternocc_vector(zcnt)=REAL(tot_noccafternocc_vector(zcnt))/tempreal
        END DO
        ! young
        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafternocc_vector_young(zcnt))+REAL(tot_occafternocc_vector_young(zcnt)))
            IF (tempreal>0.0_8) tot_occafternocc_vector_young(zcnt)=REAL(tot_occafternocc_vector_young(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafternocc_vector_young(zcnt)=REAL(tot_noccafternocc_vector_young(zcnt))/tempreal
        END DO
        ! prime
        DO zcnt=1, 4
            tempreal=(REAL(tot_noccafternocc_vector_prime(zcnt))+REAL(tot_occafternocc_vector_prime(zcnt)))
            IF (tempreal>0.0_8) tot_occafternocc_vector_prime(zcnt)=REAL(tot_occafternocc_vector_prime(zcnt))/tempreal
            IF (tempreal>0.0_8) tot_noccafternocc_vector_prime(zcnt)=REAL(tot_noccafternocc_vector_prime(zcnt))/tempreal
        END DO

    
        tempreal=(REAL(tot_corr_noccafternocc)+REAL(tot_corr_occafternocc))
        IF (tempreal>0.0_8) tot_corr_noccafternocc=REAL(tot_corr_noccafternocc)/tempreal
        IF (tempreal>0.0_8) tot_corr_occafternocc=REAL(tot_corr_occafternocc)/tempreal

    
        tempreal=(REAL(tot_corr_noccafternocc_y)+REAL(tot_corr_occafternocc_y))
        IF (tempreal>0.0_8) tot_corr_noccafternocc_y=REAL(tot_corr_noccafternocc_y)/tempreal
        IF (tempreal>0.0_8) tot_corr_occafternocc_y=REAL(tot_corr_occafternocc_y)/tempreal

        tempreal=(REAL(tot_corr_noccafternocc_p)+REAL(tot_corr_occafternocc_p))
        IF (tempreal>0.0_8) tot_corr_noccafternocc_p=REAL(tot_corr_noccafternocc_p)/tempreal
        IF (tempreal>0.0_8) tot_corr_occafternocc_p=REAL(tot_corr_occafternocc_p)/tempreal
        
        

        IF (stock_occafterocc>0.0_8) poccafterocc=REAL(poccafterocc)/REAL(stock_occafterocc)
        IF (stock_noccafterocc>0.0_8) pnoccafterocc=REAL(pnoccafterocc)/REAL(stock_noccafterocc)
        IF (stock_occafternocc>0.0_8) poccafternocc=REAL(poccafternocc)/REAL(stock_occafternocc)
        IF (stock_noccafternocc>0.0_8) pnoccafternocc=REAL(pnoccafternocc)/REAL(stock_noccafternocc)

        IF (stock_occafterocc_y>0.0_8) poccafterocc_y=REAL(poccafterocc_y)/REAL(stock_occafterocc_y)
        IF (stock_noccafterocc_y>0.0_8) pnoccafterocc_y=REAL(pnoccafterocc_y)/REAL(stock_noccafterocc_y)
        IF (stock_occafternocc_y>0.0_8) poccafternocc_y=REAL(poccafternocc_y)/REAL(stock_occafternocc_y)
        IF (stock_noccafternocc_y>0.0_8) pnoccafternocc_Y=REAL(pnoccafternocc_y)/REAL(stock_noccafternocc_y)

        IF (stock_occafterocc_p>0.0_8) poccafterocc_p=REAL(poccafterocc_p)/REAL(stock_occafterocc_p)
        IF (stock_noccafterocc_p>0.0_8) pnoccafterocc_p=REAL(pnoccafterocc_p)/REAL(stock_noccafterocc_p)
        IF (stock_occafternocc_p>0.0_8) poccafternocc_p=REAL(poccafternocc_p)/REAL(stock_occafternocc_p)
        IF (stock_noccafternocc_p>0.0_8) pnoccafternocc_p=REAL(pnoccafternocc_p)/REAL(stock_noccafternocc_p)

DO zcnt=1, 2
        IF (stock_occafterocc_uspell(zcnt)>0.0_8) poccafterocc_uspell(zcnt)=REAL(poccafterocc_uspell(zcnt))/REAL(stock_occafterocc_uspell(zcnt))
        IF (stock_noccafterocc_uspell(zcnt)>0.0_8) pnoccafterocc_uspell(zcnt)=REAL(pnoccafterocc_uspell(zcnt))/REAL(stock_noccafterocc_uspell(zcnt))
        IF (stock_occafternocc_uspell(zcnt)>0.0_8) poccafternocc_uspell(zcnt)=REAL(poccafternocc_uspell(zcnt))/REAL(stock_occafternocc_uspell(zcnt))
        IF (stock_noccafternocc_uspell(zcnt)>0.0_8) pnoccafternocc_uspell(zcnt)=REAL(pnoccafternocc_uspell(zcnt))/REAL(stock_noccafternocc_uspell(zcnt))

        IF (stock_occafterocc_y_uspell(zcnt)>0.0_8) poccafterocc_y_uspell(zcnt)=REAL(poccafterocc_y_uspell(zcnt))/REAL(stock_occafterocc_y_uspell(zcnt))
        IF (stock_noccafterocc_y_uspell(zcnt)>0.0_8) pnoccafterocc_y_uspell(zcnt)=REAL(pnoccafterocc_y_uspell(zcnt))/REAL(stock_noccafterocc_y_uspell(zcnt))
        IF (stock_occafternocc_y_uspell(zcnt)>0.0_8) poccafternocc_y_uspell(zcnt)=REAL(poccafternocc_y_uspell(zcnt))/REAL(stock_occafternocc_y_uspell(zcnt))
        IF (stock_noccafternocc_y_uspell(zcnt)>0.0_8) pnoccafternocc_y_uspell(zcnt)=REAL(pnoccafternocc_y_uspell(zcnt))/REAL(stock_noccafternocc_y_uspell(zcnt))

        IF (stock_occafterocc_p_uspell(zcnt)>0.0_8) poccafterocc_p_uspell(zcnt)=REAL(poccafterocc_p_uspell(zcnt))/REAL(stock_occafterocc_p_uspell(zcnt))
        IF (stock_noccafterocc_p_uspell(zcnt)>0.0_8) pnoccafterocc_p_uspell(zcnt)=REAL(pnoccafterocc_p_uspell(zcnt))/REAL(stock_noccafterocc_p_uspell(zcnt))
        IF (stock_occafternocc_p_uspell(zcnt)>0.0_8) poccafternocc_p_uspell(zcnt)=REAL(poccafternocc_p_uspell(zcnt))/REAL(stock_occafternocc_p_uspell(zcnt))
        IF (stock_noccafternocc_p_uspell(zcnt)>0.0_8) pnoccafternocc_p_uspell(zcnt)=REAL(pnoccafternocc_p_uspell(zcnt))/REAL(stock_noccafternocc_p_uspell(zcnt))

        noccafterocc_uspell(zcnt)=REAL(noccafterocc_uspell(zcnt))/REAL(noccafterocc_uspell(zcnt)+occafterocc_uspell(zcnt))
        noccafternocc_uspell(zcnt)=REAL(noccafternocc_uspell(zcnt))/REAL(noccafternocc_uspell(zcnt)+occafternocc_uspell(zcnt))
        occafterocc_uspell(zcnt)=1.0_8-noccafterocc_uspell(zcnt)
        occafternocc_uspell(zcnt)=1.0_8-noccafternocc_uspell(zcnt)
END DO




DO tcnt=1,tmax_qtr

        ! FOR THE DURATION REGRESSION: MOVERS
        !tempreal=REAL(data_pocc_qtr(tcnt))
        tempreal=sum(data_compduration_cocc_qtr(tcnt,1:18))
        IF(tempreal>0.0_8) data_ucompldur_occ_qtr(tcnt)=data_ucompldur_occ_qtr(tcnt)/tempreal

        tempreal=REAL(data_pocc_young_qtr(tcnt))
        IF(tempreal>0.0_8) data_ucompldur_young_occ_qtr(tcnt)=data_ucompldur_young_occ_qtr(tcnt)/tempreal

        tempreal=REAL(data_pocc_prime_qtr(tcnt))
        IF(tempreal>0.0_8) data_ucompldur_prime_occ_qtr(tcnt)=data_ucompldur_prime_occ_qtr(tcnt)/tempreal

        ! FOR THE DURATION REGRESSION: STAYERS
        tempreal=REAL(SUM(data_compduration_nocc_qtr(tcnt,1:18)))
        IF(tempreal>0.0_8) data_ucompldur_nocc_qtr(tcnt)=data_ucompldur_nocc_qtr(tcnt)/tempreal

        tempreal=REAL(data_pnocc_young_qtr(tcnt))
        IF(tempreal>0.0_8) data_ucompldur_young_nocc_qtr(tcnt)=data_ucompldur_young_nocc_qtr(tcnt)/tempreal

        tempreal=REAL(data_pnocc_prime_qtr(tcnt))
        IF(tempreal>0.0_8) data_ucompldur_prime_nocc_qtr(tcnt)=data_ucompldur_prime_nocc_qtr(tcnt)/tempreal

        tempreal=REAL(data_cocc_qtr(tcnt))
        IF (tempreal>0.0_8) data_pocc_qtr(tcnt)=REAL(data_pocc_qtr(tcnt))/tempreal

       tempreal=REAL(data_cocc_young_qtr(tcnt))
       IF (tempreal>0.0_8) data_pocc_young_qtr(tcnt)=REAL(data_pocc_young_qtr(tcnt))/tempreal
       tempreal=REAL(data_cocc_prime_qtr(tcnt))
       IF (tempreal>0.0_8) data_pocc_prime_qtr(tcnt)=REAL(data_pocc_prime_qtr(tcnt))/tempreal

        tempreal=REAL(data_nocc_qtr(tcnt))
        IF (tempreal>0.0_8) data_pnocc_qtr(tcnt)=REAL(data_pnocc_qtr(tcnt))/tempreal

        tempreal=REAL(data_nocc_young_qtr(tcnt))
        IF (tempreal>0.0_8) data_pnocc_young_qtr(tcnt)=REAL(data_pnocc_young_qtr(tcnt))/tempreal
        tempreal=REAL(data_nocc_prime_qtr(tcnt))
        IF (tempreal>0.0_8) data_pnocc_prime_qtr(tcnt)=REAL(data_pnocc_prime_qtr(tcnt))/tempreal


        tempreal=(REAL(data_cocc_qtr(tcnt))+REAL(data_nocc_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_qtr(tcnt)=REAL(data_cocc_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_young_qtr(tcnt))+REAL(data_nocc_young_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_young_qtr(tcnt)=REAL(data_cocc_young_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_prime_qtr(tcnt))+REAL(data_nocc_prime_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_prime_qtr(tcnt)=REAL(data_cocc_prime_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_inflow_qtr(tcnt))+REAL(data_nocc_inflow_qtr(tcnt)))
          IF (tempreal>0.0_8) data_cocc_inflow_qtr(tcnt)=REAL(data_cocc_inflow_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_young_inflow_qtr(tcnt))+REAL(data_nocc_young_inflow_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_young_inflow_qtr(tcnt)=REAL(data_cocc_young_inflow_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_prime_inflow_qtr(tcnt))+REAL(data_nocc_prime_inflow_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_prime_inflow_qtr(tcnt)=REAL(data_cocc_prime_inflow_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_outflow_qtr(tcnt))+REAL(data_nocc_outflow_qtr(tcnt)))
          IF (tempreal>0.0_8) data_cocc_outflow_qtr(tcnt)=REAL(data_cocc_outflow_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_young_outflow_qtr(tcnt))+REAL(data_nocc_young_outflow_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_young_outflow_qtr(tcnt)=REAL(data_cocc_young_outflow_qtr(tcnt))/tempreal

        tempreal=(REAL(data_cocc_prime_outflow_qtr(tcnt))+REAL(data_nocc_prime_outflow_qtr(tcnt)))
        IF (tempreal>0.0_8) data_cocc_prime_outflow_qtr(tcnt)=REAL(data_cocc_prime_outflow_qtr(tcnt))/tempreal


END DO

!data_nocc_qtr(:)=1.0_8-data_cocc_qtr(:)
!data_nocc_young_qtr(:)=1.0_8-data_cocc_young_qtr(:)
!data_nocc_prime_qtr(:)=1.0_8-data_cocc_prime_qtr(:)

!data_nocc_inflow_qtr(:)=1.0_8-data_cocc_inflow_qtr(:)
!data_nocc_young_inflow_qtr(:)=1.0_8-data_cocc_young_inflow_qtr(:)
!data_nocc_prime_inflow_qtr(:)=1.0_8-data_cocc_prime_inflow_qtr(:)



! TEST: write data_u_prime_qtr matrix

!      !$OMP CRITICAL
!    OPEN(UNIT=77, file='u_series.txt', status='old', form='formatted', position='append')
!    WRITE(*,*) 'UPDATING THREADNO. ------------------------------------------------------------>', threadnumber
!    WRITE(77,*) 'threadnumber==========', threadnumber
!    DO t=1, tmax_sim2
!        WRITE(77,*) data_u_prime_qtr(t), ',' ,data_prime_qtr(t)
!    END DO
!    CLOSE(77)
!      !$OMP END CRITICAL

! normalize the durationmatrix



    tempreal=REAL(tot_durationmatrix(1))
IF (tempreal>0.0) tot_durationmatrix=tot_durationmatrix/tempreal
    tempreal=REAL(tot_durationmatrix_young(1))
IF (tempreal>0.0) tot_durationmatrix_young=tot_durationmatrix_young/tempreal
    tempreal=REAL(tot_durationmatrix_prime(1))
IF (tempreal>0.0) tot_durationmatrix_prime=tot_durationmatrix_prime/tempreal

! alternative durationmatrix/survival
   tempreal=REAL(tot_durationmatrix2(1))
IF (tempreal>0.0) tot_durationmatrix2=tot_durationmatrix2/tempreal
!    tempreal=REAL(tot_durationmatrix2_young(1))
!IF (tempreal>0.0) tot_durationmatrix2_young=tot_durationmatrix2_young/tempreal
!    tempreal=REAL(tot_durationmatrix2_prime(1))
!IF (tempreal>0.0) tot_durationmatrix2_prime=tot_durationmatrix2_prime/tempreal
tempreal=REAL(tot_durationmatrix2_cocc(1))
IF (tempreal>0.0) tot_durationmatrix2_cocc=tot_durationmatrix2_cocc/tempreal
tempreal=REAL(tot_durationmatrix2_cocc_young(1))
IF (tempreal>0.0) tot_durationmatrix2_cocc_young=tot_durationmatrix2_cocc_young/tempreal
tempreal=REAL(tot_durationmatrix2_cocc_prime(1))
IF (tempreal>0.0) tot_durationmatrix2_cocc_prime=tot_durationmatrix2_cocc_prime/tempreal
tempreal=REAL(tot_durationmatrix2_nocc(1))
IF (tempreal>0.0) tot_durationmatrix2_nocc=tot_durationmatrix2_nocc/tempreal
tempreal=REAL(tot_durationmatrix2_nocc_young(1))
IF (tempreal>0.0) tot_durationmatrix2_nocc_young=tot_durationmatrix2_nocc_young/tempreal
tempreal=REAL(tot_durationmatrix2_nocc_prime(1))
IF (tempreal>0.0) tot_durationmatrix2_nocc_prime=tot_durationmatrix2_nocc_prime/tempreal

! same for data

    tempreal=REAL(tot_durationmatrix_data(1))
IF (tempreal>0.0) tot_durationmatrix_data=tot_durationmatrix_data/tempreal
    tempreal=REAL(tot_durationmatrix_young_data(1))
IF (tempreal>0.0) tot_durationmatrix_young_data=tot_durationmatrix_young_data/tempreal
    tempreal=REAL(tot_durationmatrix_prime_data(1))
IF (tempreal>0.0) tot_durationmatrix_prime_data=tot_durationmatrix_prime_data/tempreal

! UNEMPLOYMENT SURVIVAL RATES

!tot_udurationmatrix_m
    tempreal=REAL(tot_durationmatrix_cocc(1))
IF (tempreal>0.0) tot_udurationmatrix_m=tot_durationmatrix_cocc/tempreal
    tempreal=REAL(tot_durationmatrix_cocc_young(1))
IF (tempreal>0.0) tot_udurationmatrix_m_young=tot_durationmatrix_cocc_young/tempreal
    tempreal=REAL(tot_durationmatrix_cocc_prime(1))
IF (tempreal>0.0) tot_udurationmatrix_m_prime=tot_durationmatrix_cocc_prime/tempreal


!tot_udurationmatrix_s
    tempreal=REAL(tot_durationmatrix_nocc(1))
IF (tempreal>0.0) tot_udurationmatrix_s=tot_durationmatrix_nocc/tempreal
    tempreal=REAL(tot_durationmatrix_nocc_young(1))
IF (tempreal>0.0) tot_udurationmatrix_s_young=tot_durationmatrix_nocc_young/tempreal
    tempreal=REAL(tot_durationmatrix_nocc_prime(1))
IF (tempreal>0.0) tot_udurationmatrix_s_prime=tot_durationmatrix_nocc_prime/tempreal

! SAME NORMALIZATION IN THE DATA

!tot_udurationmatrix_m
    tempreal=REAL(tot_udurationmatrix_m_data(1))
IF (tempreal>0.0) tot_udurationmatrix_m_data=tot_udurationmatrix_m_data/tempreal
    tempreal=REAL(tot_udurationmatrix_m_young_data(1))
IF (tempreal>0.0) tot_udurationmatrix_m_young_data=tot_udurationmatrix_m_young_data/tempreal
    tempreal=REAL(tot_udurationmatrix_m_prime_data(1))
IF (tempreal>0.0) tot_udurationmatrix_m_prime_data=tot_udurationmatrix_m_prime_data/tempreal


!tot_udurationmatrix_s
    tempreal=REAL(tot_udurationmatrix_s_data(1))
IF (tempreal>0.0) tot_udurationmatrix_s_data=tot_udurationmatrix_s_data/tempreal
    tempreal=REAL(tot_udurationmatrix_s_young_data(1))
IF (tempreal>0.0) tot_udurationmatrix_s_young_data=tot_udurationmatrix_s_young_data/tempreal
    tempreal=REAL(tot_udurationmatrix_s_prime_data(1))
IF (tempreal>0.0) tot_udurationmatrix_s_prime_data=tot_udurationmatrix_s_prime_data/tempreal

!! HAZARDS
DO zcnt=2, 24
    tempreal=REAL(tot_durationmatrix(zcnt-1))
    IF (tempreal>0.0) tot_haz_durationmatrix(zcnt)=tot_durationmatrix(zcnt)/tempreal
    tempreal=REAL(tot_durationmatrix_young(zcnt-1))
    IF (tempreal>0.0) tot_haz_durationmatrix_young(zcnt)=tot_durationmatrix_young(zcnt)/tempreal
    tempreal=REAL(tot_durationmatrix_prime(zcnt-1))
    IF (tempreal>0.0) tot_haz_durationmatrix_prime(zcnt)=tot_durationmatrix_prime(zcnt)/tempreal


    tempreal=REAL(tot_udurationmatrix_m(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_m(zcnt)=tot_udurationmatrix_m(zcnt)/tempreal
    tempreal=REAL(tot_udurationmatrix_m_young(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_m_young(zcnt)=tot_udurationmatrix_m_young(zcnt)/tempreal
    tempreal=REAL(tot_udurationmatrix_m_prime(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_m_prime(zcnt)=tot_udurationmatrix_m_prime(zcnt)/tempreal


    tempreal=REAL(tot_udurationmatrix_s(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_s(zcnt)=tot_udurationmatrix_s(zcnt)/tempreal
    tempreal=REAL(tot_udurationmatrix_s_young(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_s_young(zcnt)=tot_udurationmatrix_s_young(zcnt)/tempreal
    tempreal=REAL(tot_udurationmatrix_s_prime(zcnt-1))
    IF (tempreal>0.0) tot_haz_udurationmatrix_s_prime(zcnt)=tot_udurationmatrix_s_prime(zcnt)/tempreal
    END DO

    ! ALTERNATIVE HAZARDS
    DO zcnt=1, 24
        IF(tot_haz_durationmatrix2(zcnt,2)>0) THEN
            tot_haz_durationmatrix2(zcnt,1)= tot_haz_durationmatrix2(zcnt,1)/tot_haz_durationmatrix2(zcnt,2)
        END IF
    END DO

    ! INCOMPLETE SPELL DISTRIBUTION
    tempreal=SUM(tot_haz_durationmatrix2(1:24,2))
    DO zcnt=1,24
        tot_haz_durationmatrix2(zcnt,2)= tot_haz_durationmatrix2(zcnt,2)/tempreal
    END DO
! COMPOSITION CM-DUR_S/M

DO zcnt=1, 18

    tempreal=REAL(tot_durationmatrix_cocc(zcnt)+tot_durationmatrix_nocc(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cocc(zcnt)=tot_durationmatrix_cocc(zcnt)/tempreal

    tempreal=REAL(tot_durationmatrix_cocc_young(zcnt)+tot_durationmatrix_nocc_young(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cocc_young(zcnt)=tot_durationmatrix_cocc_young(zcnt)/tempreal

    tempreal=REAL(tot_durationmatrix_cocc_prime(zcnt)+tot_durationmatrix_nocc_prime(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cocc_prime(zcnt)=tot_durationmatrix_cocc_prime(zcnt)/tempreal

    !
    tempreal=REAL(tot_durationmatrix_cr_cocc(zcnt)+tot_durationmatrix_cr_nocc(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cr_cocc(zcnt)=tot_durationmatrix_cr_cocc(zcnt)/tempreal

    tempreal=REAL(tot_durationmatrix_cr_cocc_young(zcnt)+tot_durationmatrix_cr_nocc_young(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cr_cocc_young(zcnt)=tot_durationmatrix_cr_cocc_young(zcnt)/tempreal

    tempreal=REAL(tot_durationmatrix_cr_cocc_prime(zcnt)+tot_durationmatrix_cr_nocc_prime(zcnt))
    IF (tempreal>0.0) tot_durationmatrix_cr_cocc_prime(zcnt)=tot_durationmatrix_cr_cocc_prime(zcnt)/tempreal


    END DO

 !WRITE(*,*) 'data_totduration_cocc_qtr(tmax_qtr/5,:)', data_totduration_cocc_qtr(tmax_qtr/4,:)
 !WRITE(*,*) 'data_totduration_nocc_qtr(tmax_qtr/5,:)', data_totduration_nocc_qtr(tmax_qtr/4,:)
                    !
                    !! DO NOT CREATE QUARTER PROFILE FIRST, THEN AVERAGE WITHIN BINS. JUST AT UP ALL OBSERVATIONS WITHIN BIN, THEN CREATE PROFILE
                    !DO pcnt=1, tmax_qtr
                    !DO zcnt=1, 18
                    !tempreal=data_totduration_cocc_qtr(pcnt, zcnt)+data_totduration_nocc_qtr(pcnt, zcnt)
                    !IF(tempreal>0.0_8) THEN
                    !    data_totduration_cocc_qtr(pcnt,zcnt)=data_totduration_cocc_qtr(pcnt,zcnt)/REAL(tempreal)
                    !    data_totduration_nocc_qtr(pcnt, zcnt)=tempreal
                    !ELSE
                    !    data_totduration_cocc_qtr(pcnt,zcnt)=0.0_8
                    !    data_totduration_nocc_qtr(pcnt,zcnt)=0.0_8
                    !END IF
                    !END DO
                    !    END DO
                    !
                    !
                    !DO pcnt=1, tmax_qtr
                    !DO zcnt=1, 18
                    !tempreal=data_totduration_cocc_young_qtr(pcnt, zcnt)+data_totduration_nocc_young_qtr(pcnt, zcnt)
                    !IF(tempreal>0.0_8) THEN
                    !    data_totduration_cocc_young_qtr(pcnt,zcnt)=data_totduration_cocc_young_qtr(pcnt,zcnt)/REAL(tempreal)
                    !    data_totduration_nocc_young_qtr(pcnt, zcnt)=tempreal
                    !ELSE
                    !    data_totduration_cocc_young_qtr(pcnt,zcnt)=0.0_8
                    !    data_totduration_nocc_young_qtr(pcnt,zcnt)=0.0_8
                    !END IF
                    !END DO
                    !    END DO
                    !
                    !DO pcnt=1, tmax_qtr
                    !DO zcnt=1, 18
                    !tempreal=data_totduration_cocc_prime_qtr(pcnt, zcnt)+data_totduration_nocc_prime_qtr(pcnt, zcnt)
                    !IF(tempreal>0.0_8) THEN
                    !    data_totduration_cocc_prime_qtr(pcnt,zcnt)=data_totduration_cocc_prime_qtr(pcnt,zcnt)/REAL(tempreal)
                    !    data_totduration_nocc_prime_qtr(pcnt, zcnt)=tempreal
                    !ELSE
                    !    data_totduration_cocc_prime_qtr(pcnt,zcnt)=0.0_8
                    !    data_totduration_nocc_prime_qtr(pcnt,zcnt)=0.0_8
                    !END IF
                    !END DO
                    !END DO
                    !

repeat_udur_corr=u_spell_crossterm/(sqrt(u_spell_before_sq)*sqrt(u_spell_now_sq))
repeat_udur_el=(u_spell_crossterm-(u_spell_before_ave*u_spell_now_ave))/((u_spell_before_sq)-(u_spell_before_ave)**2.0_8)
repeat_udur_corr_m=u_spell_crossterm_am/(sqrt(u_spell_before_am_sq)*sqrt(u_spell_now_am_sq))
repeat_udur_el_m=(u_spell_crossterm_am-(u_spell_before_am_ave*u_spell_now_am_ave))/((u_spell_before_am_sq)-(u_spell_before_am_ave)**2.0_8)
repeat_udur_corr_s=u_spell_crossterm_as/(sqrt(u_spell_before_as_sq)*sqrt(u_spell_now_as_sq))
repeat_udur_el_s=(u_spell_crossterm_as-(u_spell_before_as_ave*u_spell_now_as_ave))/((u_spell_before_as_sq)-(u_spell_before_as_ave)**2.0_8)
repeat_udur_corr_sam=u_spell_crossterm_sam/(sqrt(u_spell_before_sam_sq)*sqrt(u_spell_now_sam_sq))
repeat_udur_el_sam=(u_spell_crossterm_sam-(u_spell_before_sam_ave*u_spell_now_sam_ave))/((u_spell_before_sam_sq)-(u_spell_before_sam_ave)**2.0_8)
repeat_udur_corr_sas=u_spell_crossterm_sas/(sqrt(u_spell_before_sas_sq)*sqrt(u_spell_now_sas_sq))
repeat_udur_el_sas=(u_spell_crossterm_sas-(u_spell_before_sas_ave*u_spell_now_sas_ave))/((u_spell_before_sas_sq)-(u_spell_before_sas_ave)**2.0_8)
repeat_udur_corr_mam=u_spell_crossterm_mam/(sqrt(u_spell_before_mam_sq)*sqrt(u_spell_now_mam_sq))
repeat_udur_el_mam=(u_spell_crossterm_mam-(u_spell_before_mam_ave*u_spell_now_mam_ave))/((u_spell_before_mam_sq)-(u_spell_before_mam_ave)**2.0_8)
repeat_udur_corr_mas=u_spell_crossterm_mas/(sqrt(u_spell_before_mas_sq)*sqrt(u_spell_now_mas_sq))
repeat_udur_el_mas=(u_spell_crossterm_sas-(u_spell_before_sas_ave*u_spell_now_sas_ave))/((u_spell_before_sas_sq)-(u_spell_before_sas_ave)**2.0_8)

repeat_udur_corr_y=u_spell_young_crossterm/(sqrt(u_spell_young_before_sq)*sqrt(u_spell_young_now_sq))
repeat_udur_el_y=(u_spell_young_crossterm-(u_spell_young_before_ave*u_spell_young_now_ave))/((u_spell_young_before_sq)-(u_spell_young_before_ave)**2.0_8)
repeat_udur_corr_m_y=u_spell_young_crossterm_am/(sqrt(u_spell_young_before_am_sq)*sqrt(u_spell_young_now_am_sq))
repeat_udur_el_m_y=(u_spell_young_crossterm_am-(u_spell_young_before_am_ave*u_spell_young_now_am_ave))/((u_spell_young_before_am_sq)-(u_spell_young_before_am_ave)**2.0_8)
repeat_udur_corr_s_y=u_spell_young_crossterm_as/(sqrt(u_spell_young_before_as_sq)*sqrt(u_spell_young_now_as_sq))
repeat_udur_el_s_y=(u_spell_young_crossterm_as-(u_spell_young_before_as_ave*u_spell_young_now_as_ave))/((u_spell_young_before_as_sq)-(u_spell_young_before_as_ave)**2.0_8)
repeat_udur_corr_sam_y=u_spell_young_crossterm_sam/(sqrt(u_spell_young_before_sam_sq)*sqrt(u_spell_young_now_sam_sq))
repeat_udur_el_sam_y=(u_spell_young_crossterm_sam-(u_spell_young_before_sam_ave*u_spell_young_now_sam_ave))/((u_spell_young_before_sam_sq)-(u_spell_young_before_sam_ave)**2.0_8)
repeat_udur_corr_sas_y=u_spell_young_crossterm_sas/(sqrt(u_spell_young_before_sas_sq)*sqrt(u_spell_young_now_sas_sq))
repeat_udur_el_sas_y=(u_spell_young_crossterm_sas-(u_spell_young_before_sas_ave*u_spell_young_now_sas_ave))/((u_spell_young_before_sas_sq)-(u_spell_young_before_sas_ave)**2.0_8)
repeat_udur_corr_mam_y=u_spell_young_crossterm_mam/(sqrt(u_spell_young_before_mam_sq)*sqrt(u_spell_young_now_mam_sq))
repeat_udur_el_mam_y=(u_spell_young_crossterm_mam-(u_spell_young_before_mam_ave*u_spell_young_now_mam_ave))/((u_spell_young_before_mam_sq)-(u_spell_young_before_mam_ave)**2.0_8)
repeat_udur_corr_mas_y=u_spell_young_crossterm_mas/(sqrt(u_spell_young_before_mas_sq)*sqrt(u_spell_young_now_mas_sq))
repeat_udur_el_mas_y=(u_spell_young_crossterm_sas-(u_spell_young_before_sas_ave*u_spell_young_now_sas_ave))/((u_spell_young_before_sas_sq)-(u_spell_young_before_sas_ave)**2.0_8)


repeat_udur_corr_p=u_spell_prime_crossterm/(sqrt(u_spell_prime_before_sq)*sqrt(u_spell_prime_now_sq))
repeat_udur_el_p=(u_spell_prime_crossterm-(u_spell_prime_before_ave*u_spell_prime_now_ave))/((u_spell_prime_before_sq)-(u_spell_prime_before_ave)**2.0_8)
repeat_udur_corr_m_p=u_spell_prime_crossterm_am/(sqrt(u_spell_prime_before_am_sq)*sqrt(u_spell_prime_now_am_sq))
repeat_udur_el_m_p=(u_spell_prime_crossterm_am-(u_spell_prime_before_am_ave*u_spell_prime_now_am_ave))/((u_spell_prime_before_am_sq)-(u_spell_prime_before_am_ave)**2.0_8)
repeat_udur_corr_s_p=u_spell_prime_crossterm_as/(sqrt(u_spell_prime_before_as_sq)*sqrt(u_spell_prime_now_as_sq))
repeat_udur_el_s_p=(u_spell_prime_crossterm_as-(u_spell_prime_before_as_ave*u_spell_prime_now_as_ave))/((u_spell_prime_before_as_sq)-(u_spell_prime_before_as_ave)**2.0_8)
repeat_udur_corr_sam_p=u_spell_prime_crossterm_sam/(sqrt(u_spell_prime_before_sam_sq)*sqrt(u_spell_prime_now_sam_sq))
repeat_udur_el_sam_p=(u_spell_prime_crossterm_sam-(u_spell_prime_before_sam_ave*u_spell_prime_now_sam_ave))/((u_spell_prime_before_sam_sq)-(u_spell_prime_before_sam_ave)**2.0_8)
repeat_udur_corr_sas_p=u_spell_prime_crossterm_sas/(sqrt(u_spell_prime_before_sas_sq)*sqrt(u_spell_prime_now_sas_sq))
repeat_udur_el_sas_p=(u_spell_prime_crossterm_sas-(u_spell_prime_before_sas_ave*u_spell_prime_now_sas_ave))/((u_spell_prime_before_sas_sq)-(u_spell_prime_before_sas_ave)**2.0_8)
repeat_udur_corr_mam_p=u_spell_prime_crossterm_mam/(sqrt(u_spell_prime_before_mam_sq)*sqrt(u_spell_prime_now_mam_sq))
repeat_udur_el_mam_p=(u_spell_prime_crossterm_mam-(u_spell_prime_before_mam_ave*u_spell_prime_now_mam_ave))/((u_spell_prime_before_mam_sq)-(u_spell_prime_before_mam_ave)**2.0_8)
repeat_udur_corr_mas_p=u_spell_prime_crossterm_mas/(sqrt(u_spell_prime_before_mas_sq)*sqrt(u_spell_prime_now_mas_sq))
repeat_udur_el_mas_p=(u_spell_prime_crossterm_sas-(u_spell_prime_before_sas_ave*u_spell_prime_now_sas_ave))/((u_spell_prime_before_sas_sq)-(u_spell_prime_before_sas_ave)**2.0_8)


IF (u_spell_young_before_sam_sq==0.0_8) repeat_udur_el_sam_p=-1.0_8
IF (u_spell_young_before_mam_sq==0.0_8) repeat_udur_el_mam_p=-1.0_8
IF (u_spell_young_before_mas_sq==0.0_8) repeat_udur_el_mas_p=-1.0_8
IF (u_spell_young_before_sas_sq==0.0_8) repeat_udur_el_sas_p=-1.0_8

IF (u_spell_young_now_sam_sq==0.0_8) repeat_udur_corr_sam_p=-2.0_8
IF (u_spell_young_now_mam_sq==0.0_8) repeat_udur_corr_mam_p=-2.0_8
IF (u_spell_young_now_mas_sq==0.0_8) repeat_udur_corr_mas_p=-2.0_8
IF (u_spell_young_now_sas_sq==0.0_8) repeat_udur_corr_sas_p=-2.0_8



!Finally, normalize data_u_qtr itself.
DO t=1, tmax_qtr

    IF(data_u_qtr(t)+data_e_qtr(t)>0.0_8) THEN
        data_u_qtr(t)=data_u_qtr(t)/REAL(data_u_qtr(t)+data_e_qtr(t))
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) 'ERROR 73: u_prime>prime population: data_u_prime/data_prime:'
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) data_u_prime_qtr(t), '/', data_prime_qtr(t)
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) '-------------'
    END IF


    IF(data_uall_qtr(t)+data_e_qtr(t)>0) THEN
        data_uall_qtr(t)=data_uall_qtr(t)/(data_u_qtr(t)+data_e_qtr(t))
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) 'ERROR 73: u_prime>prime population: data_u_prime/data_prime:'
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) data_u_prime_qtr(t), '/', data_prime_qtr(t)
        !    IF (data_u_prime_qtr(t)>data_prime_qtr(t)) WRITE(59,*) '-------------'
    END IF


    IF(data_uadj18m_qtr(t)+data_e_qtr(t)>0) THEN
      data_uadj18m_qtr(t)=data_uadj18m_qtr(t)/(data_uadj18m_qtr(t)+data_e_qtr(t))
    END IF

    IF(data_uadj18m_young_qtr(t)+data_e_young_qtr(t)>0) THEN
      data_uadj18m_young_qtr(t)=data_uadj18m_young_qtr(t)/(data_uadj18m_young_qtr(t)+data_e_young_qtr(t))
    END IF


    END DO

    !==========
    ! unemployment distribution
    !==========

    !min_unemp
    min_unemp_ee=minval(data_u_qtr(1:tmax_qtr),data_u_qtr(1:tmax_qtr)>0.0_8)
    min_unemp_young_ee=minval(data_u_young_qtr(1:tmax_qtr),data_u_young_qtr(1:tmax_qtr)>0.0_8)
    min_unemp_prime_ee=minval(data_u_prime_qtr(1:tmax_qtr),data_u_prime_qtr(1:tmax_qtr)>0.0_8)
    max_unemp_ee=maxval(data_u_qtr(1:tmax_qtr))
    max_unemp_young_ee=maxval(data_u_young_qtr(1:tmax_qtr))
    max_unemp_prime_ee=maxval(data_u_prime_qtr(1:tmax_qtr))







DO t=1, tmax_qtr
    tempreal=REAL(data_ult5wk_qtr(t)+data_u5lt15wk_qtr(t)+data_u15lt27wk_qtr(t)+data_ugt27wk_qtr(t))
    IF(tempreal>0.0_8) THEN
    data_ult5wk_qtr(t)=data_ult5wk_qtr(t)/tempreal
    data_u5lt15wk_qtr(t)=data_u5lt15wk_qtr(t)/tempreal
    data_u15lt27wk_qtr(t)=data_u15lt27wk_qtr(t)/tempreal
    data_ugt27wk_qtr(t)=data_ugt27wk_qtr(t)/tempreal
    END IF
END DO


DO t=1, tmax_qtr
    tempreal=REAL(data_ult5m_qtr(t)+data_ult9m_qtr(t)+data_ult13m_qtr(t)+data_ugt13m_qtr(t))
    IF(tempreal>0.0_8) THEN
    data_ult3m_qtr(t)=data_ult3m_qtr(t)/tempreal
    data_ult5m_qtr(t)=data_ult5m_qtr(t)/tempreal
    data_ult9m_qtr(t)=data_ult9m_qtr(t)/tempreal
    data_ult13m_qtr(t)=data_ult13m_qtr(t)/tempreal
    data_ugt13m_qtr(t)=data_ugt13m_qtr(t)/tempreal
    END IF
END DO


DO t=1, tmax_qtr
    tempreal=REAL(data_ult5m_yng_qtr(t)+data_ult9m_yng_qtr(t)+data_ult13m_yng_qtr(t)+data_ugt13m_yng_qtr(t))
    IF(tempreal>0.0_8) THEN
    data_ult3m_yng_qtr(t)=data_ult3m_yng_qtr(t)/tempreal
    data_ult5m_yng_qtr(t)=data_ult5m_yng_qtr(t)/tempreal
    data_ult9m_yng_qtr(t)=data_ult9m_yng_qtr(t)/tempreal
    data_ult13m_yng_qtr(t)=data_ult13m_yng_qtr(t)/tempreal
    data_ugt13m_yng_qtr(t)=data_ugt13m_yng_qtr(t)/tempreal
    END IF
END DO

! YOUNG
DO t=1, tmax_qtr
    tempreal=REAL(data_ult5m_prm_qtr(t)+data_ult9m_prm_qtr(t)+data_ult13m_prm_qtr(t)+data_ugt13m_prm_qtr(t))
    IF(tempreal>0.0_8) THEN
    data_ult3m_prm_qtr(t)=data_ult3m_prm_qtr(t)/tempreal
    data_ult5m_prm_qtr(t)=data_ult5m_prm_qtr(t)/tempreal
    data_ult9m_prm_qtr(t)=data_ult9m_prm_qtr(t)/tempreal
    data_ult13m_prm_qtr(t)=data_ult13m_prm_qtr(t)/tempreal
    data_ugt13m_prm_qtr(t)=data_ugt13m_prm_qtr(t)/tempreal
    END IF
END DO

    !WRITE(10,*) '-unadjusted durations'
!WRITE(10,*) duration2


    !! -------------------------------------
    !!   WRITE UNEMPLOYMENT TOT_DURATIONMATRIX into duration2
    !! ---------------------------------------------------

duration2(0)=tot_durationmatrix(0)
duration2(1)=tot_durationmatrix(2)
duration2(2)=tot_durationmatrix(4)
duration2(3)=tot_durationmatrix(8)
duration2(4)=tot_durationmatrix(12)
duration2(5)=tot_durationmatrix(16)
duration2(6)=tot_durationmatrix(20)
duration2(7)=tot_durationmatrix(24)

IF(verboseswitch==1) WRITE(*,*) 'duration2= ', duration2(2)
IF(verboseswitch==1) WRITE(*,*) 'corresponding tot_durationmatrix(4)', tot_durationmatrix(4)

duration2_young(0)=tot_durationmatrix_young(0)
duration2_young(1)=tot_durationmatrix_young(2)
duration2_young(2)=tot_durationmatrix_young(4)
duration2_young(3)=tot_durationmatrix_young(8)
duration2_young(4)=tot_durationmatrix_young(12)
duration2_young(5)=tot_durationmatrix_young(16)
duration2_young(6)=tot_durationmatrix_young(20)
duration2_young(7)=tot_durationmatrix_young(24)

duration2_prime(0)=tot_durationmatrix_prime(0)
duration2_prime(1)=tot_durationmatrix_prime(2)
duration2_prime(2)=tot_durationmatrix_prime(4)
duration2_prime(3)=tot_durationmatrix_prime(8)
duration2_prime(4)=tot_durationmatrix_prime(12)
duration2_prime(5)=tot_durationmatrix_prime(16)
duration2_prime(6)=tot_durationmatrix_prime(20)
duration2_prime(7)=tot_durationmatrix_prime(24)

duration2_move(0)=tot_durationmatrix2_cocc(0)
duration2_move(1)=tot_durationmatrix2_cocc(2)
duration2_move(2)=tot_durationmatrix2_cocc(4)
duration2_move(3)=tot_durationmatrix2_cocc(8)
duration2_move(4)=tot_durationmatrix2_cocc(12)
duration2_move(5)=tot_durationmatrix2_cocc(16)
duration2_move(6)=tot_durationmatrix2_cocc(20)
duration2_move(7)=tot_durationmatrix2_cocc(24)

duration2_move_young(0)=tot_durationmatrix2_cocc_young(0)
duration2_move_young(1)=tot_durationmatrix2_cocc_young(2)
duration2_move_young(2)=tot_durationmatrix2_cocc_young(4)
duration2_move_young(3)=tot_durationmatrix2_cocc_young(8)
duration2_move_young(4)=tot_durationmatrix2_cocc_young(12)
duration2_move_young(5)=tot_durationmatrix2_cocc_young(16)
duration2_move_young(6)=tot_durationmatrix2_cocc_young(20)
duration2_move_young(7)=tot_durationmatrix2_cocc_young(24)

duration2_move_prime(0)=tot_durationmatrix2_cocc_prime(0)
duration2_move_prime(1)=tot_durationmatrix2_cocc_prime(2)
duration2_move_prime(2)=tot_durationmatrix2_cocc_prime(4)
duration2_move_prime(3)=tot_durationmatrix2_cocc_prime(8)
duration2_move_prime(4)=tot_durationmatrix2_cocc_prime(12)
duration2_move_prime(5)=tot_durationmatrix2_cocc_prime(16)
duration2_move_prime(6)=tot_durationmatrix2_cocc_prime(20)
duration2_move_prime(7)=tot_durationmatrix2_cocc_prime(24)


duration2_stay(0)=tot_durationmatrix2_nocc(0)
duration2_stay(1)=tot_durationmatrix2_nocc(2)
duration2_stay(2)=tot_durationmatrix2_nocc(4)
duration2_stay(3)=tot_durationmatrix2_nocc(8)
duration2_stay(4)=tot_durationmatrix2_nocc(12)
duration2_stay(5)=tot_durationmatrix2_nocc(16)
duration2_stay(6)=tot_durationmatrix2_nocc(20)
duration2_stay(7)=tot_durationmatrix2_nocc(24)

duration2_stay_young(0)=tot_durationmatrix2_nocc_young(0)
duration2_stay_young(1)=tot_durationmatrix2_nocc_young(2)
duration2_stay_young(2)=tot_durationmatrix2_nocc_young(4)
duration2_stay_young(3)=tot_durationmatrix2_nocc_young(8)
duration2_stay_young(4)=tot_durationmatrix2_nocc_young(12)
duration2_stay_young(5)=tot_durationmatrix2_nocc_young(16)
duration2_stay_young(6)=tot_durationmatrix2_nocc_young(20)
duration2_stay_young(7)=tot_durationmatrix2_nocc_young(24)

duration2_stay_prime(0)=tot_durationmatrix2_nocc_prime(0)
duration2_stay_prime(1)=tot_durationmatrix2_nocc_prime(2)
duration2_stay_prime(2)=tot_durationmatrix2_nocc_prime(4)
duration2_stay_prime(3)=tot_durationmatrix2_nocc_prime(8)
duration2_stay_prime(4)=tot_durationmatrix2_nocc_prime(12)
duration2_stay_prime(5)=tot_durationmatrix2_nocc_prime(16)
duration2_stay_prime(6)=tot_durationmatrix2_nocc_prime(20)
duration2_stay_prime(7)=tot_durationmatrix2_nocc_prime(24)




!! DURATION DATA

duration2_data(0)=tot_durationmatrix_data(0)
duration2_data(1)=tot_durationmatrix_data(2)
duration2_data(2)=tot_durationmatrix_data(4)
duration2_data(3)=tot_durationmatrix_data(8)
duration2_data(4)=tot_durationmatrix_data(12)
duration2_data(5)=tot_durationmatrix_data(16)
duration2_data(6)=tot_durationmatrix_data(20)
duration2_data(7)=tot_durationmatrix_data(24)

duration2_young_data(0)=tot_durationmatrix_young_data(0)
duration2_young_data(1)=tot_durationmatrix_young_data(2)
duration2_young_data(2)=tot_durationmatrix_young_data(4)
duration2_young_data(3)=tot_durationmatrix_young_data(8)
duration2_young_data(4)=tot_durationmatrix_young_data(12)
duration2_young_data(5)=tot_durationmatrix_young_data(16)
duration2_young_data(6)=tot_durationmatrix_young_data(20)
duration2_young_data(7)=tot_durationmatrix_young_data(24)

duration2_prime_data(0)=tot_durationmatrix_prime_data(0)
duration2_prime_data(1)=tot_durationmatrix_prime_data(2)
duration2_prime_data(2)=tot_durationmatrix_prime_data(4)
duration2_prime_data(3)=tot_durationmatrix_prime_data(8)
duration2_prime_data(4)=tot_durationmatrix_prime_data(12)
duration2_prime_data(5)=tot_durationmatrix_prime_data(16)
duration2_prime_data(6)=tot_durationmatrix_prime_data(20)
duration2_prime_data(7)=tot_durationmatrix_prime_data(24)


!! duration for movers and stayers


!! ADJUSTED ONE PERIOD BEFORE
tempint=1
duration2_move_data(0)=tot_durationmatrix2_cocc_data(0)
duration2_move_data(1)=tot_durationmatrix2_cocc_data(2-tempint)
duration2_move_data(2)=tot_durationmatrix2_cocc_data(4-tempint)
duration2_move_data(3)=tot_durationmatrix2_cocc_data(8-tempint)
duration2_move_data(4)=tot_durationmatrix2_cocc_data(12-tempint)
duration2_move_data(5)=tot_durationmatrix2_cocc_data(16-tempint)
duration2_move_data(6)=tot_durationmatrix2_cocc_data(20-tempint)
duration2_move_data(7)=tot_durationmatrix2_cocc_data(24-tempint)

duration2_move_young_data(0)=tot_durationmatrix2_cocc_young_data(0)
duration2_move_young_data(1)=tot_durationmatrix2_cocc_young_data(2-tempint)
duration2_move_young_data(2)=tot_durationmatrix2_cocc_young_data(4-tempint)
duration2_move_young_data(3)=tot_durationmatrix2_cocc_young_data(8-tempint)
duration2_move_young_data(4)=tot_durationmatrix2_cocc_young_data(12-tempint)
duration2_move_young_data(5)=tot_durationmatrix2_cocc_young_data(16-tempint)
duration2_move_young_data(6)=tot_durationmatrix2_cocc_young_data(20-tempint)
duration2_move_young_data(7)=tot_durationmatrix2_cocc_young_data(24-tempint)

duration2_move_prime_data(0)=tot_durationmatrix2_cocc_prime_data(0)
duration2_move_prime_data(1)=tot_durationmatrix2_cocc_prime_data(2-tempint)
duration2_move_prime_data(2)=tot_durationmatrix2_cocc_prime_data(4-tempint)
duration2_move_prime_data(3)=tot_durationmatrix2_cocc_prime_data(8-tempint)
duration2_move_prime_data(4)=tot_durationmatrix2_cocc_prime_data(12-tempint)
duration2_move_prime_data(5)=tot_durationmatrix2_cocc_prime_data(16-tempint)
duration2_move_prime_data(6)=tot_durationmatrix2_cocc_prime_data(20-tempint)
duration2_move_prime_data(7)=tot_durationmatrix2_cocc_prime_data(24-tempint)


duration2_stay_data(0)=tot_durationmatrix2_nocc_data(0)
duration2_stay_data(1)=tot_durationmatrix2_nocc_data(2-tempint)
duration2_stay_data(2)=tot_durationmatrix2_nocc_data(4-tempint)
duration2_stay_data(3)=tot_durationmatrix2_nocc_data(8-tempint)
duration2_stay_data(4)=tot_durationmatrix2_nocc_data(12-tempint)
duration2_stay_data(5)=tot_durationmatrix2_nocc_data(16-tempint)
duration2_stay_data(6)=tot_durationmatrix2_nocc_data(20-tempint)
duration2_stay_data(7)=tot_durationmatrix2_nocc_data(24-tempint)

duration2_stay_young_data(0)=tot_durationmatrix2_nocc_young_data(0)
duration2_stay_young_data(1)=tot_durationmatrix2_nocc_young_data(2-tempint)
duration2_stay_young_data(2)=tot_durationmatrix2_nocc_young_data(4-tempint)
duration2_stay_young_data(3)=tot_durationmatrix2_nocc_young_data(8-tempint)
duration2_stay_young_data(4)=tot_durationmatrix2_nocc_young_data(12-tempint)
duration2_stay_young_data(5)=tot_durationmatrix2_nocc_young_data(16-tempint)
duration2_stay_young_data(6)=tot_durationmatrix2_nocc_young_data(20-tempint)
duration2_stay_young_data(7)=tot_durationmatrix2_nocc_young_data(24-tempint)

duration2_stay_prime_data(0)=tot_durationmatrix2_nocc_prime_data(0)
duration2_stay_prime_data(1)=tot_durationmatrix2_nocc_prime_data(2-tempint)
duration2_stay_prime_data(2)=tot_durationmatrix2_nocc_prime_data(4-tempint)
duration2_stay_prime_data(3)=tot_durationmatrix2_nocc_prime_data(8-tempint)
duration2_stay_prime_data(4)=tot_durationmatrix2_nocc_prime_data(12-tempint)
duration2_stay_prime_data(5)=tot_durationmatrix2_nocc_prime_data(16-tempint)
duration2_stay_prime_data(6)=tot_durationmatrix2_nocc_prime_data(20-tempint)
duration2_stay_prime_data(7)=tot_durationmatrix2_nocc_prime_data(24-tempint)



!   WRITE(*,*) '-unadjusted E durations after occ'
!   DO counter2=0,6
!       WRITE(*,FMT='(F9.5, A1)', ADVANCE='no') empduration_occ(counter2), ','
!   END DO
!   WRITE(*,FMT='(F9.5, A1)')  empduration_occ(7)
!
   IF(empduration_occ(0)>0.0_8) THEN
   empduration_occ(1)=empduration_occ(1)/empduration_occ(0)
   empduration_occ(2)=empduration_occ(2)/empduration_occ(0)
   empduration_occ(3)=empduration_occ(3)/empduration_occ(0)
   empduration_occ(4)=empduration_occ(4)/empduration_occ(0)
   empduration_occ(5)=empduration_occ(5)/empduration_occ(0)
   empduration_occ(6)=empduration_occ(6)/empduration_occ(0)
   empduration_occ(7)=empduration_occ(7)/empduration_occ(0)
   ELSE
   empduration_occ(:)=-1.0
   END IF



   IF(empduration_nocc(0)>0.0_8) THEN
   empduration_nocc(1)=empduration_nocc(1)/empduration_nocc(0)
   empduration_nocc(2)=empduration_nocc(2)/empduration_nocc(0)
   empduration_nocc(3)=empduration_nocc(3)/empduration_nocc(0)
   empduration_nocc(4)=empduration_nocc(4)/empduration_nocc(0)
   empduration_nocc(5)=empduration_nocc(5)/empduration_nocc(0)
   empduration_nocc(6)=empduration_nocc(6)/empduration_nocc(0)
   empduration_nocc(7)=empduration_nocc(7)/empduration_nocc(0)
   ELSE
   empduration_nocc(:)=-1.0
   END IF


!
!   WRITE(*,*) '-unadjusted E durations after nocc'
!   DO counter2=0,6
!       WRITE(*,FMT='(F9.5, A1)', ADVANCE='no') empduration_nocc(counter2), ','
!   END DO
!   WRITE(*,FMT='(F9.5, A1)')  empduration_nocc(7)
!
   IF(empduration_occ_y(0)>0.0_8) THEN
   empduration_occ_y(1)=empduration_occ_y(1)/empduration_occ_y(0)
   empduration_occ_y(2)=empduration_occ_y(2)/empduration_occ_y(0)
   empduration_occ_y(3)=empduration_occ_y(3)/empduration_occ_y(0)
   empduration_occ_y(4)=empduration_occ_y(4)/empduration_occ_y(0)
   empduration_occ_y(5)=empduration_occ_y(5)/empduration_occ_y(0)
   empduration_occ_y(6)=empduration_occ_y(6)/empduration_occ_y(0)
   empduration_occ_y(7)=empduration_occ_y(7)/empduration_occ_y(0)
   ELSE
   empduration_occ_y(:)=-1.0
   END IF


   IF(empduration_nocc_y(0)>0.0_8) THEN
   empduration_nocc_y(1)=empduration_nocc_y(1)/empduration_nocc_y(0)
   empduration_nocc_y(2)=empduration_nocc_y(2)/empduration_nocc_y(0)
   empduration_nocc_y(3)=empduration_nocc_y(3)/empduration_nocc_y(0)
   empduration_nocc_y(4)=empduration_nocc_y(4)/empduration_nocc_y(0)
   empduration_nocc_y(5)=empduration_nocc_y(5)/empduration_nocc_y(0)
   empduration_nocc_y(6)=empduration_nocc_y(6)/empduration_nocc_y(0)
   empduration_nocc_y(7)=empduration_nocc_y(7)/empduration_nocc_y(0)
   ELSE
   empduration_nocc_y(:)=-1.0
   END IF

   IF(empduration_occ_p(0)>0.0_8) THEN
   empduration_occ_p(1)=empduration_occ_p(1)/empduration_occ_p(0)
   empduration_occ_p(2)=empduration_occ_p(2)/empduration_occ_p(0)
   empduration_occ_p(3)=empduration_occ_p(3)/empduration_occ_p(0)
   empduration_occ_p(4)=empduration_occ_p(4)/empduration_occ_p(0)
   empduration_occ_p(5)=empduration_occ_p(5)/empduration_occ_p(0)
   empduration_occ_p(6)=empduration_occ_p(6)/empduration_occ_p(0)
   empduration_occ_p(7)=empduration_occ_p(7)/empduration_occ_p(0)
   ELSE
   empduration_occ_p(:)=-1.0
   END IF


   IF(empduration_nocc_p(0)>0.0_8) THEN
   empduration_nocc_p(1)=empduration_nocc_p(1)/empduration_nocc_p(0)
   empduration_nocc_p(2)=empduration_nocc_p(2)/empduration_nocc_p(0)
   empduration_nocc_p(3)=empduration_nocc_p(3)/empduration_nocc_p(0)
   empduration_nocc_p(4)=empduration_nocc_p(4)/empduration_nocc_p(0)
   empduration_nocc_p(5)=empduration_nocc_p(5)/empduration_nocc_p(0)
   empduration_nocc_p(6)=empduration_nocc_p(6)/empduration_nocc_p(0)
   empduration_nocc_p(7)=empduration_nocc_p(7)/empduration_nocc_p(0)
   ELSE
   empduration_nocc_p(:)=-1.0
   END IF

   IF(empduration_occ_shuspell(0)>0.0_8) THEN
   empduration_occ_shuspell(1)=empduration_occ_shuspell(1)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(2)=empduration_occ_shuspell(2)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(3)=empduration_occ_shuspell(3)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(4)=empduration_occ_shuspell(4)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(5)=empduration_occ_shuspell(5)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(6)=empduration_occ_shuspell(6)/empduration_occ_shuspell(0)
   empduration_occ_shuspell(7)=empduration_occ_shuspell(7)/empduration_occ_shuspell(0)
   ELSE
   empduration_occ_shuspell(:)=-1.0
   END IF


   IF(empduration_nocc_shuspell(0)>0.0_8) THEN
   empduration_nocc_shuspell(1)=empduration_nocc_shuspell(1)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(2)=empduration_nocc_shuspell(2)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(3)=empduration_nocc_shuspell(3)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(4)=empduration_nocc_shuspell(4)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(5)=empduration_nocc_shuspell(5)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(6)=empduration_nocc_shuspell(6)/empduration_nocc_shuspell(0)
   empduration_nocc_shuspell(7)=empduration_nocc_shuspell(7)/empduration_nocc_shuspell(0)
   ELSE
   empduration_nocc_shuspell(:)=-1.0
   END IF

   IF(empduration_occ_luspell(0)>0.0_8) THEN
   empduration_occ_luspell(1)=empduration_occ_luspell(1)/empduration_occ_luspell(0)
   empduration_occ_luspell(2)=empduration_occ_luspell(2)/empduration_occ_luspell(0)
   empduration_occ_luspell(3)=empduration_occ_luspell(3)/empduration_occ_luspell(0)
   empduration_occ_luspell(4)=empduration_occ_luspell(4)/empduration_occ_luspell(0)
   empduration_occ_luspell(5)=empduration_occ_luspell(5)/empduration_occ_luspell(0)
   empduration_occ_luspell(6)=empduration_occ_luspell(6)/empduration_occ_luspell(0)
   empduration_occ_luspell(7)=empduration_occ_luspell(7)/empduration_occ_luspell(0)
   ELSE
   empduration_occ_luspell(:)=-1.0
   END IF


   IF(empduration_nocc_luspell(0)>0.0_8) THEN
   empduration_nocc_luspell(1)=empduration_nocc_luspell(1)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(2)=empduration_nocc_luspell(2)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(3)=empduration_nocc_luspell(3)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(4)=empduration_nocc_luspell(4)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(5)=empduration_nocc_luspell(5)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(6)=empduration_nocc_luspell(6)/empduration_nocc_luspell(0)
   empduration_nocc_luspell(7)=empduration_nocc_luspell(7)/empduration_nocc_luspell(0)
   ELSE
   empduration_nocc_luspell(:)=-1.0
   END IF

    ! DURATION DISTRIBUTION
 tempreal=ult5wk+u5lt15wk+u15lt27wk+ugt27wk
  ult5wk=ult5wk/tempreal
  u5lt15wk=u5lt15wk/tempreal
  u15lt27wk=u15lt27wk/tempreal
  ugt27wk=ugt27wk/tempreal


    !!===========================================================
    !! CALCULATE WAGE DISTRIBUTION & mM ratio
    !!===========================================================



  !! MEAN WAGES
            !


            meanwage_all=SUM(wagepz_distribution_sim(:, :))/SUM(epz_distribution_sim(:,:))
            meanwage_young=SUM(wagepz_young_distribution_sim(:, :))/SUM(epz_young_distribution_sim(:,:))
            meanwage_prime=SUM(wagepz_prime_distribution_sim(:, :))/SUM(epz_prime_distribution_sim(:,:))

            meanwage_x1=SUM(wage(:,1, :)*epz_x_distribution_sim(:,1,:))/SUM(epz_x_distribution_sim(:,1,:))
            meanwage_x2=0.0_8
            meanwage_x3=0.0_8
            IF(xpts>=2) meanwage_x2=SUM(wage(:,2, :)*epz_x_distribution_sim(:,2,:))/SUM(epz_x_distribution_sim(:,2,:))
            IF(xpts>=3) meanwage_x3=SUM(wage(:,3, :)*epz_x_distribution_sim(:,3,:))/SUM(epz_x_distribution_sim(:,3,:))

            meanwage_x1_young=SUM(wage(:,1, :)*epz_x_young_distribution_sim(:,1,:))/SUM(epz_x_young_distribution_sim(:,1,:))
            meanwage_x2_young=0.0_8
            meanwage_x3_young=0.0_8
            IF(xpts>=2) meanwage_x2_young=SUM(wage(:,2, :)*epz_x_young_distribution_sim(:,2,:))/SUM(epz_x_young_distribution_sim(:,2,:))
            IF(xpts>=3) meanwage_x3_young=SUM(wage(:,3, :)*epz_x_young_distribution_sim(:,3,:))/SUM(epz_x_young_distribution_sim(:,3,:))

            meanwage_x1_prime=SUM(wage(:,1, :)*epz_x_prime_distribution_sim(:,1,:))/SUM(epz_x_prime_distribution_sim(:,1,:))
            meanwage_x2_prime=0.0_8
            meanwage_x3_prime=0.0_8
            IF(xpts>=2) meanwage_x2_prime=SUM(wage(:,2, :)*epz_x_prime_distribution_sim(:,2,:))/SUM(epz_x_prime_distribution_sim(:,2,:))
            IF(xpts>=3) meanwage_x3_prime=SUM(wage(:,3, :)*epz_x_prime_distribution_sim(:,3,:))/SUM(epz_x_prime_distribution_sim(:,3,:))


            !! Per pcnt
            DO pcnt=1, ppts

                meanwage_all_p(pcnt)=SUM(wagepz_distribution_sim(pcnt, :))/SUM(epz_distribution_sim(pcnt,:))
                meanwage_young_p(pcnt)=SUM(wagepz_young_distribution_sim(pcnt, :))/SUM(epz_young_distribution_sim(pcnt,:))
                meanwage_prime_p(pcnt)=SUM(wagepz_prime_distribution_sim(pcnt, :))/SUM(epz_prime_distribution_sim(pcnt,:))

                meanwage_x1_p(pcnt)=SUM(wage(pcnt,1, :)*epz_x_distribution_sim(pcnt,1,:))/SUM(epz_x_distribution_sim(pcnt,1,:))
                meanwage_x2_p(pcnt)=0.0_8
                meanwage_x3_p(pcnt)=0.0_8
                IF(xpts>=2) meanwage_x2_p(pcnt)=SUM(wage(pcnt,2, :)*epz_x_distribution_sim(pcnt,2,:))/SUM(epz_x_distribution_sim(pcnt,2,:))
                IF(xpts>=3) meanwage_x3_p(pcnt)=SUM(wage(pcnt,3, :)*epz_x_distribution_sim(pcnt,3,:))/SUM(epz_x_distribution_sim(pcnt,3,:))

                meanwage_x1_young_p(pcnt)=SUM(wage(pcnt,1, :)*epz_x_young_distribution_sim(pcnt,1,:))/SUM(epz_x_young_distribution_sim(pcnt,1,:))
                meanwage_x2_young_p(pcnt)=0.0_8
                meanwage_x3_young_p(pcnt)=0.0_8
                IF(xpts>=2) meanwage_x2_young_p(pcnt)=SUM(wage(pcnt,2, :)*epz_x_young_distribution_sim(pcnt,2,:))/SUM(epz_x_young_distribution_sim(pcnt,2,:))
                IF(xpts>=3) meanwage_x3_young_p(pcnt)=SUM(wage(pcnt,3, :)*epz_x_young_distribution_sim(pcnt,3,:))/SUM(epz_x_young_distribution_sim(pcnt,3,:))

                meanwage_x1_prime_p(pcnt)=SUM(wage(pcnt,1, :)*epz_x_prime_distribution_sim(pcnt,1,:))/SUM(epz_x_prime_distribution_sim(pcnt,1,:))
                meanwage_x2_prime_p(pcnt)=0.0_8
                meanwage_x3_prime_p(pcnt)=0.0_8
                IF(xpts>=2) meanwage_x2_prime_p(pcnt)=SUM(wage(pcnt,2, :)*epz_x_prime_distribution_sim(pcnt,2,:))/SUM(epz_x_prime_distribution_sim(pcnt,2,:))
                IF(xpts>=3) meanwage_x3_prime_p(pcnt)=SUM(wage(pcnt,3, :)*epz_x_prime_distribution_sim(pcnt,3,:))/SUM(epz_x_prime_distribution_sim(pcnt,3,:))
    END DO



    !! NORMALIZE WAGE DISTRIBUTIONS

    DO pcnt=1, ppts
        DO zcnt=1, zpts

            tempreal=epz_distribution_sim(pcnt,zcnt)
            tempreal2=epz_young_distribution_sim(pcnt,zcnt)
            tempreal3=epz_prime_distribution_sim(pcnt,zcnt)


             IF(tempreal>0.0) wagepz_distribution_sim(pcnt,zcnt)=wagepz_distribution_sim(pcnt,zcnt)/tempreal
             IF(tempreal2>0.0) wagepz_young_distribution_sim(pcnt,zcnt)=wagepz_young_distribution_sim(pcnt,zcnt)/tempreal2
             IF(tempreal3>0.0) wagepz_prime_distribution_sim(pcnt,zcnt)=wagepz_prime_distribution_sim(pcnt,zcnt)/tempreal3


        END DO
    END DO

    !! RELATIVE DENSITY OF POPULATION


    epz_x2_pmass=0.0_8
    epz_x3_pmass=0.0_8
    epz_x1_pmass=SUM(epz_x_distribution_sim(:,1,:))
    IF(xpts>=2) epz_x2_pmass=SUM(epz_x_distribution_sim(:,2,:))
    IF(xpts>=2) epz_x3_pmass=SUM(epz_x_distribution_sim(:,3,:))
    tempreal=epz_x1_pmass+epz_x2_pmass+epz_x3_pmass
    IF(tempreal>0.0_8) THEN
        epz_x1_pmass=epz_x1_pmass/tempreal
        epz_x2_pmass=epz_x2_pmass/tempreal
        epz_x3_pmass=epz_x3_pmass/tempreal
    END IF

    epz_x2_young_pmass=0.0_8
    epz_x3_young_pmass=0.0_8
    epz_x1_young_pmass=SUM(epz_x_distribution_sim(:,1,:))
    IF(xpts>=2) epz_x2_young_pmass=SUM(epz_x_distribution_sim(:,2,:))
    IF(xpts>=2) epz_x3_young_pmass=SUM(epz_x_distribution_sim(:,3,:))
    tempreal=epz_x1_young_pmass+epz_x2_young_pmass+epz_x3_young_pmass
    IF(tempreal>0.0_8) THEN
        epz_x1_young_pmass=epz_x1_young_pmass/tempreal
        epz_x2_young_pmass=epz_x2_young_pmass/tempreal
        epz_x3_young_pmass=epz_x3_young_pmass/tempreal
    END IF

    epz_x2_young_pmass=0.0_8
    epz_x3_young_pmass=0.0_8
    epz_x1_young_pmass=SUM(epz_x_young_distribution_sim(:,1,:))
    IF(xpts>=2) epz_x2_young_pmass=SUM(epz_x_young_distribution_sim(:,2,:))
    IF(xpts>=2) epz_x3_young_pmass=SUM(epz_x_young_distribution_sim(:,3,:))
    tempreal=epz_x1_young_pmass+epz_x2_young_pmass+epz_x3_young_pmass
    IF(tempreal>0.0_8) THEN
        epz_x1_young_pmass=epz_x1_young_pmass/tempreal
        epz_x2_young_pmass=epz_x2_young_pmass/tempreal
        epz_x3_young_pmass=epz_x3_young_pmass/tempreal
    END IF

    epz_x2_prime_pmass=0.0_8
    epz_x3_prime_pmass=0.0_8
    epz_x1_prime_pmass=SUM(epz_x_prime_distribution_sim(:,1,:))
    IF(xpts>=2) epz_x2_prime_pmass=SUM(epz_x_prime_distribution_sim(:,2,:))
    IF(xpts>=2) epz_x3_prime_pmass=SUM(epz_x_prime_distribution_sim(:,3,:))
    tempreal=epz_x1_prime_pmass+epz_x2_prime_pmass+epz_x3_prime_pmass
    IF(tempreal>0.0_8) THEN
        epz_x1_prime_pmass=epz_x1_prime_pmass/tempreal
        epz_x2_prime_pmass=epz_x2_prime_pmass/tempreal
        epz_x3_prime_pmass=epz_x3_prime_pmass/tempreal
    END IF

    epz_young_pmass=SUM(epz_young_distribution_sim(:,:))
    epz_prime_pmass=SUM(epz_prime_distribution_sim(:,:))
    tempreal=epz_young_pmass+epz_prime_pmass
    IF(tempreal>0.0_8) epz_young_pmass=epz_young_pmass/tempreal
    IF(tempreal>0.0_8) epz_prime_pmass=epz_prime_pmass/tempreal

    ! can do this per aggregate state too
    DO pcnt=1,ppts
    epz_x2_pmass_p(pcnt)=0.0_8
    epz_x3_pmass_p(pcnt)=0.0_8
    epz_x1_pmass_p(pcnt)=SUM(epz_x_distribution_sim(pcnt,1,:))
    IF(xpts>=2) epz_x2_pmass_p(pcnt)=SUM(epz_x_distribution_sim(pcnt,2,:))
    IF(xpts>=2) epz_x3_pmass_p(pcnt)=SUM(epz_x_distribution_sim(pcnt,3,:))
    tempreal=epz_x1_pmass_p(pcnt)+epz_x2_pmass_p(pcnt)+epz_x3_pmass_p(pcnt)
    IF(tempreal>0.0_8) THEN
        epz_x1_pmass_p(pcnt)=epz_x1_pmass_p(pcnt)/tempreal
        epz_x2_pmass_p(pcnt)=epz_x2_pmass_p(pcnt)/tempreal
        epz_x3_pmass_p(pcnt)=epz_x3_pmass_p(pcnt)/tempreal
    END IF

    epz_x2_young_pmass_p(pcnt)=0.0_8
    epz_x3_young_pmass_p(pcnt)=0.0_8
    epz_x1_young_pmass_p(pcnt)=SUM(epz_x_young_distribution_sim(pcnt,1,:))
    IF(xpts>=2) epz_x2_young_pmass_p(pcnt)=SUM(epz_x_young_distribution_sim(pcnt,2,:))
    IF(xpts>=2) epz_x3_young_pmass_p(pcnt)=SUM(epz_x_young_distribution_sim(pcnt,3,:))
    tempreal=epz_x1_young_pmass_p(pcnt)+epz_x2_young_pmass_p(pcnt)+epz_x3_young_pmass_p(pcnt)
    IF(tempreal>0.0_8) THEN
        epz_x1_young_pmass_p(pcnt)=epz_x1_young_pmass_p(pcnt)/tempreal
        epz_x2_young_pmass_p(pcnt)=epz_x2_young_pmass_p(pcnt)/tempreal
        epz_x3_young_pmass_p(pcnt)=epz_x3_young_pmass_p(pcnt)/tempreal
    END IF

    epz_x2_prime_pmass_p(pcnt)=0.0_8
    epz_x3_prime_pmass_p(pcnt)=0.0_8
    epz_x1_prime_pmass_p(pcnt)=SUM(epz_x_prime_distribution_sim(pcnt,1,:))
    IF(xpts>=2) epz_x2_prime_pmass_p(pcnt)=SUM(epz_x_prime_distribution_sim(pcnt,2,:))
    IF(xpts>=2) epz_x3_prime_pmass_p(pcnt)=SUM(epz_x_prime_distribution_sim(pcnt,3,:))
    tempreal=epz_x1_prime_pmass_p(pcnt)+epz_x2_prime_pmass_p(pcnt)+epz_x3_prime_pmass_p(pcnt)
    IF(tempreal>0.0_8) THEN
        epz_x1_prime_pmass_p(pcnt)=epz_x1_prime_pmass_p(pcnt)/tempreal
        epz_x2_prime_pmass_p(pcnt)=epz_x2_prime_pmass_p(pcnt)/tempreal
        epz_x3_prime_pmass_p(pcnt)=epz_x3_prime_pmass_p(pcnt)/tempreal
    END IF

    epz_young_pmass_p(pcnt)=SUM(epz_young_distribution_sim(pcnt,:))
    epz_prime_pmass_p(pcnt)=SUM(epz_prime_distribution_sim(pcnt,:))
    tempreal=epz_young_pmass_p(pcnt)+epz_prime_pmass_p(pcnt)
    IF(tempreal>0.0_8) epz_young_pmass_p(pcnt)=epz_young_pmass_p(pcnt)/tempreal
    IF(tempreal>0.0_8) epz_prime_pmass_p(pcnt)=epz_prime_pmass_p(pcnt)/tempreal

    END DO

    !epz_x1_pmass_p, epz_x2_pmass_p, epz_x3_pmass_p, epz_x1_young_pmass_p, epz_x2_young_pmass_p,&
    !epz_x3_young_pmass_p, epz_x1_prime_pmass_p, epz_x2_prime_pmass_p, epz_x3_prime_pmass_p, &
    !epz_young_pmass_p, epz_prime_pmass_p

    !!----------------------------------------
    !! NORMALIZE (UN) EMPLOYMENT DISTRIBUTIONS

    epz_xallp_distribution_sim=epz_x_distribution_sim/SUM(epz_x_distribution_sim(:,:,:))
    epz_xallp_young_distribution_sim=epz_x_young_distribution_sim/SUM(epz_x_young_distribution_sim(:,:,:))
    epz_xallp_prime_distribution_sim=epz_x_prime_distribution_sim/SUM(epz_x_prime_distribution_sim(:,:,:))

    ! all workers
normalize_do1: &
 DO pcnt=1, ppts

    up_rest_distribution_sim(pcnt)=up_rest_distribution_sim(pcnt)/REAL(up_distribution_sim(pcnt)+ep_distribution_sim(pcnt))
    up_reall_distribution_sim(pcnt)=up_reall_distribution_sim(pcnt)/REAL(up_distribution_sim(pcnt)+ep_distribution_sim(pcnt))
    up_search_distribution_sim(pcnt)=up_search_distribution_sim(pcnt)/REAL(up_distribution_sim(pcnt)+ep_distribution_sim(pcnt))

    up_distribution_sim(pcnt)=up_distribution_sim(pcnt)/REAL(up_distribution_sim(pcnt)+ep_distribution_sim(pcnt))
    uallp_distribution_sim(pcnt)=uallp_distribution_sim(pcnt)/REAL(uallp_distribution_sim(pcnt)+eallp_distribution_sim(pcnt))
    !! ALL WORKERS
    tempreal=sum(upz_distribution_sim(pcnt, :)+epz_distribution_sim(pcnt, :))
    tempreal2=sum(upz_distribution_sim(pcnt, :))
    tempreal3=sum(epz_distribution_sim(pcnt, :))

    tempreal4=sum(epz_x_distribution_sim(pcnt, 1,:))
    IF(xpts>=2) tempreal5=sum(epz_x_distribution_sim(pcnt, 2,:))
    IF(xpts>=3)  tempreal6=sum(epz_x_distribution_sim(pcnt, 3,:))

    IF (tempreal>0.0_8) THEN
        up_distribution_sim2(pcnt)=sum(upz_distribution_sim(pcnt,:))/REAL(tempreal)
        upz_distribution_sim(pcnt,:)=upz_distribution_sim(pcnt,:)/REAL(tempreal)
        epz_distribution_sim(pcnt,:)=epz_distribution_sim(pcnt,:)/REAL(tempreal)

        DO xcnt=1,xpts
            epz_x_distribution_sim(pcnt,xcnt,:)=epz_x_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
            upz_x_distribution_sim(pcnt,xcnt,:)=upz_x_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
        END DO


    END IF
    !! YOUNG
    tempreal=sum(upz_young_distribution_sim(pcnt, :)+epz_young_distribution_sim(pcnt, :))
    tempreal2=sum(upz_young_distribution_sim(pcnt, :))
    tempreal3=sum(epz_young_distribution_sim(pcnt, :))
    tempreal4=sum(epz_x_young_distribution_sim(pcnt, 1,:))
    IF(xpts>=2) tempreal5=sum(epz_x_young_distribution_sim(pcnt, 2,:))
    IF(xpts>=3)  tempreal6=sum(epz_x_young_distribution_sim(pcnt, 3,:))


    IF (tempreal>0.0_8) THEN
        up_young_distribution_sim(pcnt)=sum(upz_young_distribution_sim(pcnt,:))/REAL(tempreal)
        upz_young_distribution_sim(pcnt,:)=upz_young_distribution_sim(pcnt,:)/REAL(tempreal)
        epz_young_distribution_sim(pcnt,:)=epz_young_distribution_sim(pcnt,:)/REAL(tempreal)

        DO xcnt=1,xpts
            epz_x_young_distribution_sim(pcnt,xcnt,:)=epz_x_young_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
            upz_x_young_distribution_sim(pcnt,xcnt,:)=upz_x_young_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
        END DO

        up_young_rest_distribution_sim(pcnt)=up_young_rest_distribution_sim(pcnt)/tempreal
        up_young_reall_distribution_sim(pcnt)=up_young_reall_distribution_sim(pcnt)/tempreal
        up_young_search_distribution_sim(pcnt)=up_young_search_distribution_sim(pcnt)/tempreal

    END IF


    tempreal=sum(upz_prime_distribution_sim(pcnt, :)+epz_prime_distribution_sim(pcnt,:))
    tempreal2=sum(upz_prime_distribution_sim(pcnt, :))
    tempreal3=sum(epz_prime_distribution_sim(pcnt, :))
    tempreal4=sum(epz_x_prime_distribution_sim(pcnt, 1,:))
    IF(xpts>=2) tempreal5=sum(epz_x_prime_distribution_sim(pcnt, 2,:))
    IF(xpts>=3)  tempreal6=sum(epz_x_prime_distribution_sim(pcnt, 3,:))

    IF (tempreal>0.0_8) THEN
        up_prime_distribution_sim(pcnt)=sum(upz_prime_distribution_sim(pcnt,:))/REAL(tempreal)
        upz_prime_distribution_sim(pcnt,:)=upz_prime_distribution_sim(pcnt,:)/REAL(tempreal)
        epz_prime_distribution_sim(pcnt,:)=epz_prime_distribution_sim(pcnt,:)/REAL(tempreal)

        DO xcnt=1,xpts
            epz_x_prime_distribution_sim(pcnt,xcnt,:)=epz_x_prime_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
            upz_x_prime_distribution_sim(pcnt,xcnt,:)=upz_x_prime_distribution_sim(pcnt,xcnt,:)/REAL(tempreal)
        END DO

        up_prime_rest_distribution_sim(pcnt)=up_prime_rest_distribution_sim(pcnt)/tempreal
        up_prime_reall_distribution_sim(pcnt)=up_prime_reall_distribution_sim(pcnt)/tempreal
        up_prime_search_distribution_sim(pcnt)=up_prime_search_distribution_sim(pcnt)/tempreal

    END IF


    !IF (tempreal>0.0_8) up_prime_distribution_sim(pcnt)=sum(upz_prime_distribution_sim(pcnt,:))/tempreal
    !IF (tempreal2>0.0_8) upz_prime_distribution_sim(pcnt,:)=upz_prime_distribution_sim(pcnt,:)/REAL(tempreal)
    !IF (tempreal3>0.0_8) epz_prime_distribution_sim(pcnt,:)=epz_prime_distribution_sim(pcnt,:)/REAL(tempreal)
    !IF(tempreal4>0.0_8) epz_x_prime_distribution_sim(pcnt,1,:)=epz_x_prime_distribution_sim(pcnt,1,:)/REAL(tempreal)
    !IF(xpts>=2 .and. tempreal5>0.0_8) epz_x_prime_distribution_sim(pcnt,2,:)=epz_x_prime_distribution_sim(pcnt,2,:)/REAL(tempreal)
    !IF(xpts>=3 .and. tempreal6>0.0_8) epz_x_prime_distribution_sim(pcnt,3,:)=epz_x_prime_distribution_sim(pcnt,3,:)/REAL(tempreal)




    tempreal=sum(epz_1yb4_x_distribution_sim(pcnt,:, :))
    IF(tempreal>0.0_8) THEN
        epz_1yb4_x_distribution_sim(pcnt,:,:)=epz_1yb4_x_distribution_sim(pcnt,:,:)/REAL(tempreal)
    END IF

    ! DONE IN STATA?
    !DO zcnt=1, zpts
    !    epz_1yb4_distribution_sim(pcnt,:)=sum(epz_1yb4_x_distribution_sim(pcnt,zcnt,:))
    !END DO

    tempreal2=sum(sep_epz_1yb4_x_distribution_sim(pcnt,:, :))
    IF(tempreal2>0.0_8) THEN
        sep_epz_1yb4_x_distribution_sim(pcnt,:,:)=sep_epz_1yb4_x_distribution_sim(pcnt,:,:)/REAL(tempreal)
    END IF


    !

END DO normalize_do1




    !!----------------------------------------
    !! PERCENTILE


            ! SET THOSE VARIABLES TO ZERO THAT DO NOT NECESSARILY ARE FILLED IN
            m0mratio_x2=0.0_8
            m0mratio_x3=0.0_8
            m0mratio_x2_young=0.0_8
            m0mratio_x3_young=0.0_8
            m0mratio_x2_prime=0.0_8
            m0mratio_x3_prime=0.0_8
            m0mratio_x2_p=0.0_8
            m0mratio_x3_p=0.0_8
            m0mratio_x2_young_p=0.0_8
            m0mratio_x3_young_p=0.0_8
            m0mratio_x2_prime_p=0.0_8
            m0mratio_x3_prime_p=0.0_8

            m1mratio_x2=0.0_8
            m1mratio_x3=0.0_8
            m1mratio_x2_young=0.0_8
            m1mratio_x3_young=0.0_8
            m1mratio_x2_prime=0.0_8
            m1mratio_x3_prime=0.0_8
            m1mratio_x2_p=0.0_8
            m1mratio_x3_p=0.0_8
            m1mratio_x2_young_p=0.0_8
            m1mratio_x3_young_p=0.0_8
            m1mratio_x2_prime_p=0.0_8
            m1mratio_x3_prime_p=0.0_8


            m5mratio_x2=0.0_8
            m5mratio_x3=0.0_8
            m5mratio_x2_young=0.0_8
            m5mratio_x3_young=0.0_8
            m5mratio_x2_prime=0.0_8
            m5mratio_x3_prime=0.0_8
            m5mratio_x2_p=0.0_8
            m5mratio_x3_p=0.0_8
            m5mratio_x2_young_p=0.0_8
            m5mratio_x3_young_p=0.0_8
            m5mratio_x2_prime_p=0.0_8
            m5mratio_x3_prime_p=0.0_8

            m10mratio_x2=0.0_8
            m10mratio_x3=0.0_8
            m10mratio_x2_young=0.0_8
            m10mratio_x3_young=0.0_8
            m10mratio_x2_prime=0.0_8
            m10mratio_x3_prime=0.0_8
            m10mratio_x2_p=0.0_8
            m10mratio_x3_p=0.0_8
            m10mratio_x2_young_p=0.0_8
            m10mratio_x3_young_p=0.0_8
            m10mratio_x2_prime_p=0.0_8
            m10mratio_x3_prime_p=0.0_8


            !m0mratio_all_p(pcnt)=wquantile(0.0_8,pack(wagepz_distribution_sim(:,:), epz_distribution_sim(:,:)>0.0_8) , pack(epz_distribution_sim(:,:), epz_distribution_sim(:,:)>0.0_8))
            !m0mratio_young_p(pcnt)=wquantile(0.0_8,pack(wagepz_young_distribution_sim(:,:), epz_young_distribution_sim(:,:)>0.0_8) , pack(epz_young_distribution_sim(:,:), epz_young_distribution_sim(:,:)>0.0_8))
            !m0mratio_prime_p(pcnt)=wquantile(0.0_8,pack(wagepz_prime_distribution_sim(:,:), epz_prime_distribution_sim(:,:)>0.0_8) , pack(epz_prime_distribution_sim(:,:), epz_prime_distribution_sim(:,:)>0.0_8))

            m0mratio_all=wquantile(0.0_8,pack(wage(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8))
            m0mratio_x1=wquantile(0.0_8,pack(wage(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2=wquantile(0.0_8,pack(wage(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3=wquantile(0.0_8,pack(wage(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8))

            m0mratio_young=wquantile(0.0_8,pack(wage(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8))
            m0mratio_x1_young=wquantile(0.0_8,pack(wage(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2_young=wquantile(0.0_8,pack(wage(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3_young=wquantile(0.0_8,pack(wage(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8))

            m0mratio_prime=wquantile(0.0_8,pack(wage(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8))
            m0mratio_x1_prime=wquantile(0.0_8,pack(wage(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2_prime=wquantile(0.0_8,pack(wage(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3_prime=wquantile(0.0_8,pack(wage(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8))


        DO pcnt=1, ppts
            m0mratio_all_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8))
            m0mratio_x1_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8))

            m0mratio_young_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8))
            m0mratio_x1_young_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2_young_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3_young_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8))

            m0mratio_prime_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8))
            m0mratio_x1_prime_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m0mratio_x2_prime_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m0mratio_x3_prime_p(pcnt)=wquantile(0.0_8,pack(wage(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8))
    END DO

    !! 1st percentile


            m1mratio_all=wquantile(0.01_8,pack(wage(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8))
            m1mratio_x1=wquantile(0.01_8,pack(wage(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2=wquantile(0.01_8,pack(wage(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3=wquantile(0.01_8,pack(wage(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8))

            m1mratio_young=wquantile(0.01_8,pack(wage(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8))
            m1mratio_x1_young=wquantile(0.01_8,pack(wage(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2_young=wquantile(0.01_8,pack(wage(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3_young=wquantile(0.01_8,pack(wage(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8))

            m1mratio_prime=wquantile(0.01_8,pack(wage(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8))
            m1mratio_x1_prime=wquantile(0.01_8,pack(wage(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2_prime=wquantile(0.01_8,pack(wage(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3_prime=wquantile(0.01_8,pack(wage(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8))


        DO pcnt=1, ppts
            m1mratio_all_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8))
            m1mratio_x1_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8))

            m1mratio_young_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8))
            m1mratio_x1_young_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2_young_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3_young_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8))

            m1mratio_prime_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8))
            m1mratio_x1_prime_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m1mratio_x2_prime_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m1mratio_x3_prime_p(pcnt)=wquantile(0.01_8,pack(wage(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8))
    END DO

        !! 5th percentile

            m5mratio_all=wquantile(0.05_8,pack(wage(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8))
            m5mratio_x1=wquantile(0.05_8,pack(wage(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2=wquantile(0.05_8,pack(wage(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3=wquantile(0.05_8,pack(wage(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8))

            m5mratio_young=wquantile(0.05_8,pack(wage(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8))
            m5mratio_x1_young=wquantile(0.05_8,pack(wage(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2_young=wquantile(0.05_8,pack(wage(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3_young=wquantile(0.05_8,pack(wage(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8))

            m5mratio_prime=wquantile(0.05_8,pack(wage(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8))
            m5mratio_x1_prime=wquantile(0.05_8,pack(wage(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2_prime=wquantile(0.05_8,pack(wage(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3_prime=wquantile(0.05_8,pack(wage(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8))


        DO pcnt=1, ppts
            m5mratio_all_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8))
            m5mratio_x1_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8))

            m5mratio_young_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8))
            m5mratio_x1_young_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2_young_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3_young_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8))

            m5mratio_prime_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8))
            m5mratio_x1_prime_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m5mratio_x2_prime_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m5mratio_x3_prime_p(pcnt)=wquantile(0.05_8,pack(wage(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8))
    END DO

        !! 10th percentile

            m10mratio_all=wquantile(0.1_8,pack(wage(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,:,:), epz_xallp_distribution_sim(:,:,:)>0.0_8))
            m10mratio_x1=wquantile(0.1_8,pack(wage(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,1,:), epz_xallp_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2=wquantile(0.1_8,pack(wage(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,2,:), epz_xallp_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3=wquantile(0.1_8,pack(wage(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8) , pack(epz_xallp_distribution_sim(:,3,:), epz_xallp_distribution_sim(:,3,:)>0.0_8))

            m10mratio_young=wquantile(0.1_8,pack(wage(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,:,:), epz_x_young_distribution_sim(:,:,:)>0.0_8))
            m10mratio_x1_young=wquantile(0.1_8,pack(wage(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,1,:), epz_x_young_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2_young=wquantile(0.1_8,pack(wage(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,2,:), epz_x_young_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3_young=wquantile(0.1_8,pack(wage(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(:,3,:), epz_x_young_distribution_sim(:,3,:)>0.0_8))

            m10mratio_prime=wquantile(0.1_8,pack(wage(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,:,:), epz_x_prime_distribution_sim(:,:,:)>0.0_8))
            m10mratio_x1_prime=wquantile(0.1_8,pack(wage(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,1,:), epz_x_prime_distribution_sim(:,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2_prime=wquantile(0.1_8,pack(wage(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,2,:), epz_x_prime_distribution_sim(:,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3_prime=wquantile(0.1_8,pack(wage(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(:,3,:), epz_x_prime_distribution_sim(:,3,:)>0.0_8))


        DO pcnt=1, ppts
            m10mratio_all_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8))
            m10mratio_x1_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,1,:), epz_x_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,2,:), epz_x_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,3,:), epz_x_distribution_sim(pcnt,3,:)>0.0_8))

            m10mratio_young_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,:,:), epz_x_young_distribution_sim(pcnt,:,:)>0.0_8))
            m10mratio_x1_young_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,1,:), epz_x_young_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2_young_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,2,:), epz_x_young_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3_young_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_young_distribution_sim(pcnt,3,:), epz_x_young_distribution_sim(pcnt,3,:)>0.0_8))

            m10mratio_prime_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,:,:), epz_x_prime_distribution_sim(pcnt,:,:)>0.0_8))
            m10mratio_x1_prime_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,1,:), epz_x_prime_distribution_sim(pcnt,1,:)>0.0_8))
            IF(xpts>=2) m10mratio_x2_prime_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,2,:), epz_x_prime_distribution_sim(pcnt,2,:)>0.0_8))
            IF(xpts>=3) m10mratio_x3_prime_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8) , pack(epz_x_prime_distribution_sim(pcnt,3,:), epz_x_prime_distribution_sim(pcnt,3,:)>0.0_8))
    END DO



    !! DIVISION TO GET TO mM ratio
            m0mratio_all=meanwage_all/m0mratio_all
            m0mratio_young=meanwage_young/m0mratio_young
            m0mratio_prime=meanwage_prime/m0mratio_prime

            m0mratio_x1=meanwage_x1/m0mratio_x1
            IF(xpts>=2) m0mratio_x2=meanwage_x2/m0mratio_x2
            IF(xpts>=3) m0mratio_x3=meanwage_x3/m0mratio_x3

            m0mratio_x1_young=meanwage_x1_young/m0mratio_x1_young
            IF(xpts>=2) m0mratio_x2_young=meanwage_x2_young/m0mratio_x2_young
            IF(xpts>=3) m0mratio_x3_young=meanwage_x3_young/m0mratio_x3_young

            m0mratio_x1_prime=meanwage_x1_prime/m0mratio_x1_prime
            IF(xpts>=2) m0mratio_x2_prime=meanwage_x2_prime/m0mratio_x2_prime
            IF(xpts>=3) m0mratio_x3_prime=meanwage_x3_prime/m0mratio_x3_prime

            !! Per pcnt
            DO pcnt=1, ppts

                    m0mratio_all_p(pcnt)=meanwage_all_p(pcnt)/m0mratio_all_p(pcnt)
                    m0mratio_young_p(pcnt)=meanwage_young_p(pcnt)/m0mratio_young_p(pcnt)
                    m0mratio_prime_p(pcnt)=meanwage_prime_p(pcnt)/m0mratio_prime_p(pcnt)

                    m0mratio_x1_p(pcnt)=meanwage_x1_p(pcnt)/m0mratio_x1_p(pcnt)
                    IF(xpts>=2) m0mratio_x2_p(pcnt)=meanwage_x2_p(pcnt)/m0mratio_x2_p(pcnt)
                    IF(xpts>=3) m0mratio_x3_p(pcnt)=meanwage_x3_p(pcnt)/m0mratio_x3_p(pcnt)

                    m0mratio_x1_young_p(pcnt)=meanwage_x1_young_p(pcnt)/m0mratio_x1_young_p(pcnt)
                    IF(xpts>=2) m0mratio_x2_young_p(pcnt)=meanwage_x2_young_p(pcnt)/m0mratio_x2_young_p(pcnt)
                    IF(xpts>=3) m0mratio_x3_young_p(pcnt)=meanwage_x3_young_p(pcnt)/m0mratio_x3_young_p(pcnt)

                    m0mratio_x1_prime_p(pcnt)=meanwage_x1_prime_p(pcnt)/m0mratio_x1_prime_p(pcnt)
                    IF(xpts>=2) m0mratio_x2_prime_p(pcnt)=meanwage_x2_prime_p(pcnt)/m0mratio_x2_prime_p(pcnt)
                    IF(xpts>=3) m0mratio_x3_prime_p(pcnt)=meanwage_x3_prime_p(pcnt)/m0mratio_x3_prime_p(pcnt)

    END DO

            !! 1st percentile Mm ratio

    !! DIVISION TO GET TO mM ratio
            m1mratio_all=meanwage_all/m1mratio_all
            m1mratio_young=meanwage_young/m1mratio_young
            m1mratio_prime=meanwage_prime/m1mratio_prime

            m1mratio_x1=meanwage_x1/m1mratio_x1
            IF(xpts>=2) m1mratio_x2=meanwage_x2/m1mratio_x2
            IF(xpts>=3) m1mratio_x3=meanwage_x3/m1mratio_x3

            m1mratio_x1_young=meanwage_x1_young/m1mratio_x1_young
            IF(xpts>=2) m1mratio_x2_young=meanwage_x2_young/m1mratio_x2_young
            IF(xpts>=3) m1mratio_x3_young=meanwage_x3_young/m1mratio_x3_young

            m1mratio_x1_prime=meanwage_x1_prime/m1mratio_x1_prime
            IF(xpts>=2) m1mratio_x2_prime=meanwage_x2_prime/m1mratio_x2_prime
            IF(xpts>=3) m1mratio_x3_prime=meanwage_x3_prime/m1mratio_x3_prime

            !! Per pcnt
            DO pcnt=1, ppts

                    m1mratio_all_p(pcnt)=meanwage_all_p(pcnt)/m1mratio_all_p(pcnt)
                    m1mratio_young_p(pcnt)=meanwage_young_p(pcnt)/m1mratio_young_p(pcnt)
                    m1mratio_prime_p(pcnt)=meanwage_prime_p(pcnt)/m1mratio_prime_p(pcnt)

                    m1mratio_x1_p(pcnt)=meanwage_x1_p(pcnt)/m1mratio_x1_p(pcnt)
                    IF(xpts>=2) m1mratio_x2_p(pcnt)=meanwage_x2_p(pcnt)/m1mratio_x2_p(pcnt)
                    IF(xpts>=3) m1mratio_x3_p(pcnt)=meanwage_x3_p(pcnt)/m1mratio_x3_p(pcnt)

                    m1mratio_x1_young_p(pcnt)=meanwage_x1_young_p(pcnt)/m1mratio_x1_young_p(pcnt)
                    IF(xpts>=2) m1mratio_x2_young_p(pcnt)=meanwage_x2_young_p(pcnt)/m1mratio_x2_young_p(pcnt)
                    IF(xpts>=3) m1mratio_x3_young_p(pcnt)=meanwage_x3_young_p(pcnt)/m1mratio_x3_young_p(pcnt)

                    m1mratio_x1_prime_p(pcnt)=meanwage_x1_prime_p(pcnt)/m1mratio_x1_prime_p(pcnt)
                    IF(xpts>=2) m1mratio_x2_prime_p(pcnt)=meanwage_x2_prime_p(pcnt)/m1mratio_x2_prime_p(pcnt)
                    IF(xpts>=3) m1mratio_x3_prime_p(pcnt)=meanwage_x3_prime_p(pcnt)/m1mratio_x3_prime_p(pcnt)

    END DO

            !! 5th percentile Mm ratio

    !! DIVISION TO GET TO mM ratio
            m5mratio_all=meanwage_all/m5mratio_all
            m5mratio_young=meanwage_young/m5mratio_young
            m5mratio_prime=meanwage_prime/m5mratio_prime

            m5mratio_x1=meanwage_x1/m5mratio_x1
            IF(xpts>=2) m5mratio_x2=meanwage_x2/m5mratio_x2
            IF(xpts>=3) m5mratio_x3=meanwage_x3/m5mratio_x3

            m5mratio_x1_young=meanwage_x1_young/m5mratio_x1_young
            IF(xpts>=2) m5mratio_x2_young=meanwage_x2_young/m5mratio_x2_young
            IF(xpts>=3) m5mratio_x3_young=meanwage_x3_young/m5mratio_x3_young

            m5mratio_x1_prime=meanwage_x1_prime/m5mratio_x1_prime
            IF(xpts>=2) m5mratio_x2_prime=meanwage_x2_prime/m5mratio_x2_prime
            IF(xpts>=3) m5mratio_x3_prime=meanwage_x3_prime/m5mratio_x3_prime

            !! Per pcnt
            DO pcnt=1, ppts

                    m5mratio_all_p(pcnt)=meanwage_all_p(pcnt)/m5mratio_all_p(pcnt)
                    m5mratio_young_p(pcnt)=meanwage_young_p(pcnt)/m5mratio_young_p(pcnt)
                    m5mratio_prime_p(pcnt)=meanwage_prime_p(pcnt)/m5mratio_prime_p(pcnt)

                    m5mratio_x1_p(pcnt)=meanwage_x1_p(pcnt)/m5mratio_x1_p(pcnt)
                    IF(xpts>=2) m5mratio_x2_p(pcnt)=meanwage_x2_p(pcnt)/m5mratio_x2_p(pcnt)
                    IF(xpts>=3) m5mratio_x3_p(pcnt)=meanwage_x3_p(pcnt)/m5mratio_x3_p(pcnt)

                    m5mratio_x1_young_p(pcnt)=meanwage_x1_young_p(pcnt)/m5mratio_x1_young_p(pcnt)
                    IF(xpts>=2) m5mratio_x2_young_p(pcnt)=meanwage_x2_young_p(pcnt)/m5mratio_x2_young_p(pcnt)
                    IF(xpts>=3) m5mratio_x3_young_p(pcnt)=meanwage_x3_young_p(pcnt)/m5mratio_x3_young_p(pcnt)

                    m5mratio_x1_prime_p(pcnt)=meanwage_x1_prime_p(pcnt)/m5mratio_x1_prime_p(pcnt)
                    IF(xpts>=2) m5mratio_x2_prime_p(pcnt)=meanwage_x2_prime_p(pcnt)/m5mratio_x2_prime_p(pcnt)
                    IF(xpts>=3) m5mratio_x3_prime_p(pcnt)=meanwage_x3_prime_p(pcnt)/m5mratio_x3_prime_p(pcnt)

    END DO


    !! DIVISION TO GET TO mM ratio
            m10mratio_all=meanwage_all/m10mratio_all
            m10mratio_young=meanwage_young/m10mratio_young
            m10mratio_prime=meanwage_prime/m10mratio_prime

            m10mratio_x1=meanwage_x1/m10mratio_x1
            IF(xpts>=2) m10mratio_x2=meanwage_x2/m10mratio_x2
            IF(xpts>=3) m10mratio_x3=meanwage_x3/m10mratio_x3

            m10mratio_x1_young=meanwage_x1_young/m10mratio_x1_young
            IF(xpts>=2) m10mratio_x2_young=meanwage_x2_young/m10mratio_x2_young
            IF(xpts>=3) m10mratio_x3_young=meanwage_x3_young/m10mratio_x3_young

            m10mratio_x1_prime=meanwage_x1_prime/m10mratio_x1_prime
            IF(xpts>=2) m10mratio_x2_prime=meanwage_x2_prime/m10mratio_x2_prime
            IF(xpts>=3) m10mratio_x3_prime=meanwage_x3_prime/m10mratio_x3_prime

            !! Per pcnt
            DO pcnt=1, ppts

                    m10mratio_all_p(pcnt)=meanwage_all_p(pcnt)/m10mratio_all_p(pcnt)
                    m10mratio_young_p(pcnt)=meanwage_young_p(pcnt)/m10mratio_young_p(pcnt)
                    m10mratio_prime_p(pcnt)=meanwage_prime_p(pcnt)/m10mratio_prime_p(pcnt)

                    m10mratio_x1_p(pcnt)=meanwage_x1_p(pcnt)/m10mratio_x1_p(pcnt)
                    IF(xpts>=2) m10mratio_x2_p(pcnt)=meanwage_x2_p(pcnt)/m10mratio_x2_p(pcnt)
                    IF(xpts>=3) m10mratio_x3_p(pcnt)=meanwage_x3_p(pcnt)/m10mratio_x3_p(pcnt)

                    m10mratio_x1_young_p(pcnt)=meanwage_x1_young_p(pcnt)/m10mratio_x1_young_p(pcnt)
                    IF(xpts>=2) m10mratio_x2_young_p(pcnt)=meanwage_x2_young_p(pcnt)/m10mratio_x2_young_p(pcnt)
                    IF(xpts>=3) m10mratio_x3_young_p(pcnt)=meanwage_x3_young_p(pcnt)/m10mratio_x3_young_p(pcnt)

                    m10mratio_x1_prime_p(pcnt)=meanwage_x1_prime_p(pcnt)/m10mratio_x1_prime_p(pcnt)
                    IF(xpts>=2) m10mratio_x2_prime_p(pcnt)=meanwage_x2_prime_p(pcnt)/m10mratio_x2_prime_p(pcnt)
                    IF(xpts>=3) m10mratio_x3_prime_p(pcnt)=meanwage_x3_prime_p(pcnt)/m10mratio_x3_prime_p(pcnt)

    END DO


    !! MUELLER MOMENTS
     DO pcnt=1, ppts
         prod_median(pcnt)=wquantile(0.5_8,pack(prod(pcnt,:,:), epz_1yb4_x_distribution_sim(pcnt,:,:)>0.0_8) , &
                                    pack(epz_1yb4_x_distribution_sim(pcnt,:,:), epz_1yb4_x_distribution_sim(pcnt,:,:)>0.0_8))
         wage_median(pcnt)=wquantile(0.5_8,pack(wage(pcnt,:,:), epz_1yb4_x_distribution_sim(pcnt,:,:)>0.0_8) , &
                                    pack(epz_1yb4_x_distribution_sim(pcnt,:,:), epz_1yb4_x_distribution_sim(pcnt,:,:)>0.0_8))
         !m10mratio_all_p(pcnt)=wquantile(0.1_8,pack(wage(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8) , pack(epz_x_distribution_sim(pcnt,:,:), epz_x_distribution_sim(pcnt,:,:)>0.0_8))


    !tempreal=
    sep_prod_above_med(pcnt)=SUM(pack(sep_epz_1yb4_x_distribution_sim(pcnt,:,:), prod(pcnt,:,:)>prod_median(pcnt)))/SUM(pack(epz_1yb4_x_distribution_sim(pcnt,:,:), prod(pcnt,:,:)>prod_median(pcnt)))
    sep_prod_below_med(pcnt)=SUM(pack(sep_epz_1yb4_x_distribution_sim(pcnt,:,:), prod(pcnt,:,:)<=prod_median(pcnt)))/SUM(pack(epz_1yb4_x_distribution_sim(pcnt,:,:), prod(pcnt,:,:)<=prod_median(pcnt)))
    sep_wage_above_med(pcnt)=SUM(pack(sep_epz_1yb4_x_distribution_sim(pcnt,:,:), wage(pcnt,:,:)>wage_median(pcnt)))/SUM(pack(epz_1yb4_x_distribution_sim(pcnt,:,:), wage(pcnt,:,:)>wage_median(pcnt)))
    sep_wage_below_med(pcnt)=SUM(pack(sep_epz_1yb4_x_distribution_sim(pcnt,:,:), wage(pcnt,:,:)<=wage_median(pcnt)))/SUM(pack(epz_1yb4_x_distribution_sim(pcnt,:,:), wage(pcnt,:,:)<=wage_median(pcnt)))

    END DO



    !!======================
    !!!   RENORMALIZE THE EMPLOYMENT AND UNEMPLOYMENT DISTRIBUTION
    !!!!===========================

    !! FIRST THE (P,Z) DISTRIBUTION

    !
    !DO pcnt=1, ppts
    !
    !
    !        tempreal=SUM(epz_x_distribution_sim(pcnt,1,:))
    !        IF(xpts>=2) tempreal2=SUM(epz_x_distribution_sim(pcnt,2,:))
    !        IF(xpts>=3) tempreal3=SUM(epz_x_distribution_sim(pcnt,3,:))
    !
    !             IF(tempreal>0.0) epz_x_distribution_sim(pcnt,1,:)=epz_x_distribution_sim(pcnt,1,:)/tempreal
    !             IF(xpts>=2 .and. tempreal2>0.0) epz_x_distribution_sim(pcnt,2,:)=epz_x_distribution_sim(pcnt,2,:)/tempreal2
    !             IF(xpts>=3 .and. tempreal3>0.0) epz_x_distribution_sim(pcnt,3,:)=epz_x_distribution_sim(pcnt,3,:)/tempreal3
    !
    !        tempreal=SUM(epz_x_young_distribution_sim(pcnt,1,:))
    !        IF(xpts>=2) tempreal2=SUM(epz_x_young_distribution_sim(pcnt,2,:))
    !        IF(xpts>=3) tempreal3=SUM(epz_x_young_distribution_sim(pcnt,3,:))
    !
    !             IF(tempreal>0.0) epz_x_young_distribution_sim(pcnt,1,:)=epz_x_young_distribution_sim(pcnt,1,:)/tempreal
    !             IF(xpts>=2 .and. tempreal2>0.0) epz_x_young_distribution_sim(pcnt,2,:)=epz_x_young_distribution_sim(pcnt,2,:)/tempreal2
    !             IF(xpts>=3 .and. tempreal3>0.0) epz_x_young_distribution_sim(pcnt,3,:)=epz_x_young_distribution_sim(pcnt,3,:)/tempreal3
    !
    !        tempreal=SUM(epz_x_prime_distribution_sim(pcnt,1,:))
    !        IF(xpts>=2) tempreal2=SUM(epz_x_prime_distribution_sim(pcnt,2,:))
    !        IF(xpts>=3) tempreal3=SUM(epz_x_prime_distribution_sim(pcnt,3,:))
    !
    !             IF(tempreal>0.0) epz_x_prime_distribution_sim(pcnt,1,:)=epz_x_prime_distribution_sim(pcnt,1,:)/tempreal
    !             IF(xpts>=2 .and. tempreal2>0.0) epz_x_prime_distribution_sim(pcnt,2,:)=epz_x_prime_distribution_sim(pcnt,2,:)/tempreal2
    !             IF(xpts>=3 .and. tempreal3>0.0) epz_x_prime_distribution_sim(pcnt,3,:)=epz_x_prime_distribution_sim(pcnt,3,:)/tempreal3
    !END DO

    !! CALCULATE AVERAGE MM RATIOS

    m0mratio_age_contr=epz_young_pmass*m0mratio_young+epz_prime_pmass*m0mratio_prime
    m0mratio_age_x_contr=epz_young_pmass*(epz_x1_young_pmass*m0mratio_x1_young + epz_x2_young_pmass*m0mratio_x2_young + epz_x3_young_pmass*m0mratio_x3_young) &
                            + epz_prime_pmass*(epz_x1_prime_pmass*m0mratio_x1_prime + epz_x2_prime_pmass*m0mratio_x2_prime + epz_x3_prime_pmass*m0mratio_x3_prime)

    m0mratio_age_p_contr=0.0_8
    DO pcnt=1, ppts
        m0mratio_age_p_contr=m0mratio_age_p_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(m0mratio_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(m0mratio_prime_p(pcnt)))
    END DO

    m0mratio_age_p_x_contr=0.0_8
    IF(xpts==3) THEN
    DO pcnt=1, ppts
        m0mratio_age_p_x_contr=m0mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m0mratio_x1_young_p(pcnt) + epz_x2_young_pmass_p(pcnt)*m0mratio_x2_young_p(pcnt) &
                                        + epz_x3_young_pmass_p(pcnt)*m0mratio_x3_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m0mratio_x1_prime_p(pcnt) + epz_x2_prime_pmass_p(pcnt)*m0mratio_x2_prime_p(pcnt) &
                                    + epz_x3_prime_pmass_p(pcnt)*m0mratio_x3_prime_p(pcnt)))
    END DO
    END IF
    IF(xpts==1) THEN
    m0mratio_age_p_x_contr=0.0_8
    DO pcnt=1, ppts
        m0mratio_age_p_x_contr=m0mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m0mratio_x1_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m0mratio_x1_prime_p(pcnt)))
    END DO
    END IF

    ! 1st ptile
    m1mratio_age_contr=epz_young_pmass*m1mratio_young+epz_prime_pmass*m1mratio_prime
    m1mratio_age_x_contr=epz_young_pmass*(epz_x1_young_pmass*m1mratio_x1_young + epz_x2_young_pmass*m1mratio_x2_young + epz_x3_young_pmass*m1mratio_x3_young) &
                            + epz_prime_pmass*(epz_x1_prime_pmass*m1mratio_x1_prime + epz_x2_prime_pmass*m1mratio_x2_prime + epz_x3_prime_pmass*m1mratio_x3_prime)

    m1mratio_age_p_contr=0.0_8
    DO pcnt=1, ppts
        m1mratio_age_p_contr=m1mratio_age_p_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(m1mratio_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(m1mratio_prime_p(pcnt)))
    END DO


    m1mratio_age_p_x_contr=0.0_8
    IF(xpts==3) THEN
    DO pcnt=1, ppts
        m1mratio_age_p_x_contr=m1mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m1mratio_x1_young_p(pcnt) + epz_x2_young_pmass_p(pcnt)*m1mratio_x2_young_p(pcnt) &
                                        + epz_x3_young_pmass_p(pcnt)*m1mratio_x3_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m1mratio_x1_prime_p(pcnt) + epz_x2_prime_pmass_p(pcnt)*m1mratio_x2_prime_p(pcnt) &
                                    + epz_x3_prime_pmass_p(pcnt)*m1mratio_x3_prime_p(pcnt)))
    END DO
    END IF
    IF(xpts==1) THEN
    m1mratio_age_p_x_contr=0.0_8
    DO pcnt=1, ppts
        m1mratio_age_p_x_contr=m1mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m1mratio_x1_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m1mratio_x1_prime_p(pcnt)))
    END DO
    END IF


    ! 5th ptile
    m5mratio_age_contr=epz_young_pmass*m5mratio_young+epz_prime_pmass*m5mratio_prime
    m5mratio_age_x_contr=epz_young_pmass*(epz_x1_young_pmass*m5mratio_x1_young + epz_x2_young_pmass*m5mratio_x2_young + epz_x3_young_pmass*m5mratio_x3_young) &
                            + epz_prime_pmass*(epz_x1_prime_pmass*m5mratio_x1_prime + epz_x2_prime_pmass*m5mratio_x2_prime + epz_x3_prime_pmass*m5mratio_x3_prime)

    m5mratio_age_p_contr=0.0_8
    DO pcnt=1, ppts
        m5mratio_age_p_contr=m5mratio_age_p_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(m5mratio_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(m5mratio_prime_p(pcnt)))
    END DO


    m5mratio_age_p_x_contr=0.0_8
    IF(xpts==3) THEN
    DO pcnt=1, ppts
        m5mratio_age_p_x_contr=m5mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m5mratio_x1_young_p(pcnt) + epz_x2_young_pmass_p(pcnt)*m5mratio_x2_young_p(pcnt) &
                                        + epz_x3_young_pmass_p(pcnt)*m5mratio_x3_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m5mratio_x1_prime_p(pcnt) + epz_x2_prime_pmass_p(pcnt)*m5mratio_x2_prime_p(pcnt) &
                                    + epz_x3_prime_pmass_p(pcnt)*m5mratio_x3_prime_p(pcnt)))
    END DO
    END IF
    IF(xpts==1) THEN
    m5mratio_age_p_x_contr=0.0_8
    DO pcnt=1, ppts
        m5mratio_age_p_x_contr=m5mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m5mratio_x1_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m5mratio_x1_prime_p(pcnt)))
    END DO
    END IF


    m10mratio_age_contr=epz_young_pmass*m10mratio_young+epz_prime_pmass*m10mratio_prime
    m10mratio_age_x_contr=epz_young_pmass*(epz_x1_young_pmass*m10mratio_x1_young + epz_x2_young_pmass*m10mratio_x2_young + epz_x3_young_pmass*m10mratio_x3_young) &
                            + epz_prime_pmass*(epz_x1_prime_pmass*m10mratio_x1_prime + epz_x2_prime_pmass*m10mratio_x2_prime + epz_x3_prime_pmass*m10mratio_x3_prime)
    m10mratio_age_p_contr=0.0_8
    DO pcnt=1, ppts
        m10mratio_age_p_contr=m10mratio_age_p_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(m10mratio_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(m10mratio_prime_p(pcnt)))
    END DO

    m10mratio_age_p_x_contr=0.0_8
    IF(xpts==3) THEN
    DO pcnt=1, ppts
        m10mratio_age_p_x_contr=m10mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m10mratio_x1_young_p(pcnt) + epz_x2_young_pmass_p(pcnt)*m10mratio_x2_young_p(pcnt) &
                                        + epz_x3_young_pmass_p(pcnt)*m10mratio_x3_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m10mratio_x1_prime_p(pcnt) + epz_x2_prime_pmass_p(pcnt)*m10mratio_x2_prime_p(pcnt) &
                                    + epz_x3_prime_pmass_p(pcnt)*m10mratio_x3_prime_p(pcnt)))
    END DO
    END IF
    IF(xpts==1) THEN
    m10mratio_age_p_x_contr=0.0_8
    DO pcnt=1, ppts
        m10mratio_age_p_x_contr=m10mratio_age_p_x_contr+&
            p_distribution_sim(pcnt)*(epz_young_pmass_p(pcnt)*(epz_x1_young_pmass_p(pcnt)*m10mratio_x1_young_p(pcnt)) &
                                    + epz_prime_pmass_p(pcnt)*(epz_x1_prime_pmass_p(pcnt)*m10mratio_x1_prime_p(pcnt)))
    END DO
    END IF

    !up_distribution_sim(:)=    up_distribution_sim(:)/(REAL(nsim_gen)*REAL(tmax_sim2))
    !up_young_distribution_sim(:)=    up_young_distribution_sim(:)/sum(data_young_qtr(:))
    !up_prime_distribution_sim(:)=    up_prime_distribution_sim(:)/(REAL(nsim_gen)*REAL(tmax_sim2)-sum(data_young_qtr(:)))
!    IF (threadnumber==1) THEN
!    t=4000
!        WRITE(*,*) 'at t, u, v,theta, jf, jfo, sep, wage prod'
!        WRITE(*,FMT='(I5, 8(F8.3,X) )') t, data_u_qtr(t), data_v_qtr(t), data_theta_qtr(t), data_jf_qtr(t),data_jfo_qtr(t), &
!        data_sep_qtr(t), data_wage_qtr(t), data_prod_qtr(t)
!    END IF !(threadnumber==1)

        !------------------------------------
        !   DERIVED MOMENTS
        !------------------------------------

!IF (threadnumber==1) THEN
!CLOSE(49)
!CLOSE(50)
!CLOSE(51)
!CLOSE(53)
!END IF ! (threadnumber==1)


! IF (threadnumber==1 .AND. verboseswitchfile==1) THEN
! OPEN(UNIT=93, file='monthlyseries2.txt', form='formatted', status='replace')
! WRITE(93, FMT='(8(A8,A3), A8)', ADVANCE='no') 'unempl1', ',' , 'unempl2', ',-,', 'vacancy1', ',' , 'vacancy2', ',-,',&
! 'tightns1', ',', 'tightns2', ',-,', 'jbfind1', ',', 'jbfind2', ',-,' , 'no.agnts'
! WRITE(93, FMT='(6(A8,A3))', ADVANCE='no') 'sep rate', ',' , 'sepr2', ',-,' ,'prod', ',', 'prod2', ',-,' , 'wage', ',' , 'wage2', ',-,'
! WRITE(93, FMT='(3(A8,A3))') 'cinfl', ',' , 'ninfl', ',', 'aggprod'
!
! DO t=1, tmax_qtr
!   WRITE(93, FMT='(8(F8.4,A3), I6, A3)', ADVANCE='no') data_u_qtr(t), ',' , data_u_qtr(t), ',|,' ,&
!             data_v_qtr(t), ',' , data_v_qtr(t), ',|,' ,data_theta_qtr(t), ',' , data_theta_qtr(t), ',|,' , &
!             data_jf_qtr(t), ',' , data_jf_qtr(t), ',|,' , number_agents(t), ',|,'
!
!   WRITE(93, FMT='(6(F7.5,A3))',ADVANCE='no') data_sep_qtr(t), ',' , data_sep_qtr(t), ',|,' , data_prod_qtr(t), ',' , data_prod_qtr(t), ',|,' , &
!              0.0_8, ',' , 0.0_8, ',|,'
!   WRITE(93, FMT='(4(F10.3,A3))',ADVANCE='no') data_cocc_inflow_qtr(t), ',' , data_nocc_inflow_qtr(t), ',|,' ! , data_temp_qtr(t), ',|,'  , data_temp_qtr2(t), ',|,'
!   WRITE(93, FMT='(F8.3)') pvector(aggprod_ind(t))
! END DO
! WRITE(93, *) 'tot_dur(0)_cocc=', tot_durationmatrix_cocc(0), 'tot_dur(0)_nocc=', tot_durationmatrix_nocc(0)
! CLOSE(93)
! END IF

!  !$OMP CRITICAL
!DEALLOCATE(data_temp_qtr, data_temp_qtr2)
!  !$OMP END CRITICAL

!=======
!!USE THE LAST tmax_sim observations of tmax_sim2 series
!============

IF (tmax_sim2>tmax_sim) THEN
        WRITE(*,*) '---- tmax_sim/tmax_sim2 discordance----'
END IF



!===================================================================
!  SOME QUARTERLY AVERAGES (from weekly data), OVERALL
!===================================================================


sepocc_ave=sepocc_ave/REAL(sepocc_ave_counter)
sepnocc_ave=sepnocc_ave/REAL(sepnocc_ave_counter)
sepocc_y_ave=sepocc_y_ave/REAL(sepocc_y_ave_counter)
sepnocc_y_ave=sepnocc_y_ave/REAL(sepnocc_y_ave_counter)
sepocc_p_ave=sepocc_p_ave/REAL(sepocc_p_ave_counter)
sepnocc_p_ave=sepnocc_p_ave/REAL(sepnocc_p_ave_counter)

    unemp_ave=sum(data_u_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    uall_ave=sum(data_uall_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    uadj18m_ave=sum(data_uadj18m_qtr(1:tmax_qtr))/REAL(tmax_qtr)

    uadj18m_young_ave=sum(data_uadj18m_young_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    u_young_ave=sum(data_u_young_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    u_prime_ave=sum(data_u_prime_qtr(1:tmax_qtr))/REAL(tmax_qtr)

    cocc_ave=sum(data_cocc_outflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    cocc_young_ave=sum(data_cocc_young_outflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    cocc_prime_ave=sum(data_cocc_prime_outflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)

    !cocc_inflow_ave=sum(data_cocc_inflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    !cocc_young_inflow_ave=sum(data_cocc_young_inflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    !cocc_prime_inflow_ave=sum(data_cocc_prime_inflow_qtr(1:tmax_qtr))/REAL(tmax_qtr)

    cocc_inflow_ave=tot_durationmatrix_cocc(1)
    cocc_young_inflow_ave=tot_durationmatrix_cocc_young(1)
    cocc_prime_inflow_ave=tot_durationmatrix_cocc_prime(1)

    !== UNEMPLOYMENT DURATION BY OCCUPATIONAL STAY OR MOVE
    udur_occ_ave_q=sum(data_ucompldur_occ_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    udur_nocc_ave_q=sum(data_ucompldur_nocc_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    udur_occ_young_ave_q=sum(data_ucompldur_young_occ_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    udur_nocc_young_ave_q=sum(data_ucompldur_young_nocc_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    udur_occ_prime_ave_q=sum(data_ucompldur_prime_occ_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    udur_nocc_prime_ave_q=sum(data_ucompldur_prime_nocc_qtr(1:tmax_qtr))/REAL(tmax_qtr)

    !== UNEMPLOYMENT DURATION BY OCCUPATIONAL STAY OR MOVE -- CENSORED.
    !   = censored: lower bound 1-6 months, upper bound 16

   udur_occ_with_hplu=0.0 
   udur_occ_with_lu=0.0 
   udur_occ_with_u=0.0 
   udur_occ_with_hpu=0.0 
   
   udur_nocc_with_u=0.0 
   udur_nocc_with_hpu=0.0
   udur_nocc_with_hplu=0.0 
   udur_nocc_with_lu=0.0
   
   udur_occ_young_with_hplu=0.0 
   udur_occ_young_with_lu=0.0
   udur_occ_young_with_u=0.0 
   udur_occ_young_with_hpu=0.0 
   
   udur_nocc_young_with_u=0.0 
   udur_nocc_young_with_hpu=0.0
   udur_nocc_young_with_hplu=0.0 
   udur_nocc_young_with_lu=0.0
   
   udur_nocc_prime_with_hplu=0.0 
   udur_nocc_prime_with_lu=0.0 
   udur_nocc_prime_with_u=0.0 
   udur_nocc_prime_with_hpu=0.0
   
   udur_occ_prime_with_u=0.0 
   udur_occ_prime_with_hpu=0.0 
   udur_occ_prime_with_hplu=0.0 
   udur_occ_prime_with_lu=0.0
   
    
    udur_mvec_occ_ave=0.0_8
    udur_mvec_occ_young_ave=0.0_8
    udur_mvec_occ_prime_ave=0.0_8
    udur_mvec_nocc_ave=0.0_8
    udur_mvec_nocc_young_ave=0.0_8
    udur_mvec_nocc_prime_ave=0.0_8

    DO tcnt=1, 6

    tempreal=0.0_8
    tempreal2=0.0_8
    tempreal3=0.0_8
    tempreal4=0.0_8
    tempreal5=0.0_8
    tempreal6=0.0_8

        DO zcnt=tcnt, 18
            tempreal=tempreal+(data_compduration_cocc(zcnt))*REAL(zcnt)
            tempreal2=tempreal2+(data_compduration_cocc_young(zcnt))*REAL(zcnt)
            tempreal3=tempreal3+(data_compduration_cocc_prime(zcnt))*REAL(zcnt)
            tempreal4=tempreal4+(data_compduration_nocc(zcnt))*REAL(zcnt)
            tempreal5=tempreal5+(data_compduration_nocc_young(zcnt))*REAL(zcnt)
            tempreal6=tempreal6+(data_compduration_nocc_prime(zcnt))*REAL(zcnt)
        END DO ! zcnt
    udur_mvec_occ_ave(tcnt)=tempreal/sum(data_compduration_cocc(tcnt:18))
    udur_mvec_occ_young_ave(tcnt)=tempreal2/sum(data_compduration_cocc_young(tcnt:18))
    udur_mvec_occ_prime_ave(tcnt)=tempreal3/sum(data_compduration_cocc_prime(tcnt:18))
    udur_mvec_nocc_ave(tcnt)=tempreal4/sum(data_compduration_nocc(tcnt:18))
    udur_mvec_nocc_young_ave(tcnt)=tempreal5/sum(data_compduration_nocc_young(tcnt:18))
    udur_mvec_nocc_prime_ave(tcnt)=tempreal6/sum(data_compduration_nocc_prime(tcnt:18))
    END DO ! tcnt




    !!! CHECK WHICH VERSION TO TAKE
    !noccafterocc_ave=sum(tot_noccafterocc_vector(2:4))/3.0_8
    !noccafternocc_ave=sum(tot_noccafternocc_vector(2:4))/3.0_8
    !noccafternocc_ave=tot_noccafternocc_vector(1)
    !noccafterocc_y_ave=sum(tot_noccafterocc_vector_young(2:4))/3.0_8
    !noccafternocc_y_ave=sum(tot_noccafternocc_vector_young(2:4))/3.0_8
    !noccafterocc_p_ave=sum(tot_noccafterocc_vector_prime(2:4))/3.0_8
    !noccafternocc_p_ave=sum(tot_noccafternocc_vector_prime(2:4))/3.0_8
    !noccafterall_ave=1.0-(sum(tot_durationmatrix_cocc(2:4))/3.0_8)
    !noccafterall_y_ave=1.0-(sum(tot_durationmatrix_cocc_young(2:4))/3.0_8)
    !noccafterall_p_ave=1.0-(sum(tot_durationmatrix_cocc_prime(2:4))/3.0_8)

    !noccafterocc_ave=(tot_noccafterocc_vector(2))
    !noccafternocc_ave=(tot_noccafternocc_vector(2))    !noccafternocc_ave=tot_noccafternocc_vector(2)
    !noccafterocc_y_ave=(tot_noccafterocc_vector_young(2))
    !noccafternocc_y_ave=(tot_noccafternocc_vector_young(2))
    !noccafterocc_p_ave=(tot_noccafterocc_vector_prime(2))
    !noccafternocc_p_ave=(tot_noccafternocc_vector_prime(2))
    !noccafterall_ave=1.0-(tot_durationmatrix_cocc(2))
    !noccafterall_y_ave=1.0-(tot_durationmatrix_cocc_young(2))
    !noccafterall_p_ave=1.0-(tot_durationmatrix_cocc_prime(2))


    noccafterocc_ave=(tot_noccafterocc_vector(1))
    noccafternocc_ave=(tot_noccafternocc_vector(1))
    noccafterocc_y_ave=(tot_noccafterocc_vector_young(1))
    noccafternocc_y_ave=(tot_noccafternocc_vector_young(1))
    noccafterocc_p_ave=(tot_noccafterocc_vector_prime(1))
    noccafternocc_p_ave=(tot_noccafternocc_vector_prime(1))
    noccafterall_ave=1.0-(tot_durationmatrix_cocc(1))
    noccafterall_y_ave=1.0-(tot_durationmatrix_cocc_young(1))
    noccafterall_p_ave=1.0-(tot_durationmatrix_cocc_prime(1))


    v_ave=sum(data_v_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    theta_ave=sum(data_theta_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    ! NOTE: DIFFERENCE IS TIME AGGREGATION.
                ! jf_ave is average quarterly reate
                ! jf_ave2 is average individual job finding rate over the sample, weighs much more heavily recessions (more unemployment)
    jf_ave=sum(data_jf_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jf_ave2=jf_ave2/REAL(jf_ave2_counter)
    !WRITE(*,*) 'jf ave', jf_ave

    jf_young_ave=sum(data_jf_young_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jf_prime_ave=sum(data_jf_prime_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfocc_ave=sum(data_pocc_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfocc_young_ave=sum(data_pocc_young_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfocc_prime_ave=sum(data_pocc_prime_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfnocc_ave=sum(data_pnocc_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfnocc_young_ave=sum(data_pnocc_young_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    jfnocc_prime_ave=sum(data_pnocc_prime_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    sep_ave=sum(data_sep_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    sepyoung_ave=sum(data_sep_y_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    sepprime_ave=sum(data_sep_p_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    prod_ave=sum(data_prod_qtr(1:tmax_qtr))/REAL(tmax_qtr)
    wage_ave=sum(data_wage_qtr(1:tmax_qtr))/REAL(tmax_qtr)

   uprop_3m_ave=sum(data_ult3m_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_5m_ave=sum(data_ult5m_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_9m_ave=sum(data_ult9m_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_13m_ave=sum(data_ult13m_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_g13m_ave=sum(data_ugt13m_qtr(1:tmax_qtr))/REAL(tmax_qtr)

   uprop_yng_3m_ave=sum(data_ult3m_yng_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_yng_5m_ave=sum(data_ult5m_yng_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_yng_9m_ave=sum(data_ult9m_yng_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_yng_13m_ave=sum(data_ult13m_yng_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_yng_g13m_ave=sum(data_ugt13m_yng_qtr(1:tmax_qtr))/REAL(tmax_qtr)

   uprop_prm_3m_ave=sum(data_ult3m_prm_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_prm_5m_ave=sum(data_ult5m_prm_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_prm_9m_ave=sum(data_ult9m_prm_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_prm_13m_ave=sum(data_ult13m_prm_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   uprop_prm_g13m_ave=sum(data_ugt13m_prm_qtr(1:tmax_qtr))/REAL(tmax_qtr)

   ult5wk_ave=sum(data_ult5wk_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   u5lt15wk_ave=sum(data_u5lt15wk_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   u15lt27wk_ave=sum(data_u15lt27wk_qtr(1:tmax_qtr))/REAL(tmax_qtr)
   ugt27wk_ave=sum(data_ugt27wk_qtr(1:tmax_qtr))/REAL(tmax_qtr)


! LOG THE DURATIONS; THERE COULD BE ZEROES THAT MESS UP THE LOG CALCULATION!

data_l_ult5wk_qtr(1:tmax_qtr)=LOG(data_ult5wk_qtr(1:tmax_qtr))
data_l_u5lt15wk_qtr(1:tmax_qtr)=LOG(data_u5lt15wk_qtr(1:tmax_qtr))
data_l_u15lt27wk_qtr(1:tmax_qtr)=LOG(data_u15lt27wk_qtr(1:tmax_qtr))
data_l_ugt27wk_qtr(1:tmax_qtr)=LOG(data_ugt27wk_qtr(1:tmax_qtr))

data_l_ult3m_qtr(1:tmax_qtr)=LOG(data_ult3m_qtr(1:tmax_qtr))
data_l_ult5m_qtr(1:tmax_qtr)=LOG(data_ult5m_qtr(1:tmax_qtr))
data_l_ult9m_qtr(1:tmax_qtr)=LOG(data_ult9m_qtr(1:tmax_qtr))
data_l_ult13m_qtr(1:tmax_qtr)=LOG(data_ult13m_qtr(1:tmax_qtr))
data_l_ugt13m_qtr(1:tmax_qtr)=LOG(data_ugt13m_qtr(1:tmax_qtr))

data_l_ult3m_yng_qtr(1:tmax_qtr)=LOG(data_ult3m_yng_qtr(1:tmax_qtr))
data_l_ult5m_yng_qtr(1:tmax_qtr)=LOG(data_ult5m_yng_qtr(1:tmax_qtr))
data_l_ult9m_yng_qtr(1:tmax_qtr)=LOG(data_ult9m_yng_qtr(1:tmax_qtr))
data_l_ult13m_yng_qtr(1:tmax_qtr)=LOG(data_ult13m_yng_qtr(1:tmax_qtr))
data_l_ugt13m_yng_qtr(1:tmax_qtr)=LOG(data_ugt13m_yng_qtr(1:tmax_qtr))

data_l_ult3m_prm_qtr(1:tmax_qtr)=LOG(data_ult3m_prm_qtr(1:tmax_qtr))
data_l_ult5m_prm_qtr(1:tmax_qtr)=LOG(data_ult5m_prm_qtr(1:tmax_qtr))
data_l_ult9m_prm_qtr(1:tmax_qtr)=LOG(data_ult9m_prm_qtr(1:tmax_qtr))
data_l_ult13m_prm_qtr(1:tmax_qtr)=LOG(data_ult13m_prm_qtr(1:tmax_qtr))
data_l_ugt13m_prm_qtr(1:tmax_qtr)=LOG(data_ugt13m_prm_qtr(1:tmax_qtr))



!calculate variances
var_uprop_3m_ave=VARCALCUL(data_ult3m_qtr, tmax_qtr)
var_uprop_5m_ave=VARCALCUL(data_ult5m_qtr, tmax_qtr)
var_uprop_9m_ave=VARCALCUL(data_ult9m_qtr, tmax_qtr)
var_uprop_13m_ave=VARCALCUL(data_ult13m_qtr, tmax_qtr)
var_uprop_g13m_ave=VARCALCUL(data_ugt13m_qtr, tmax_qtr)
var_l_uprop_3m_ave=VARCALCUL(data_l_ult3m_qtr, tmax_qtr)
var_l_uprop_5m_ave=VARCALCUL(data_l_ult5m_qtr, tmax_qtr)
var_l_uprop_9m_ave=VARCALCUL(data_l_ult9m_qtr, tmax_qtr)
var_l_uprop_13m_ave=VARCALCUL(data_l_ult13m_qtr, tmax_qtr)
var_l_uprop_g13m_ave=VARCALCUL(data_l_ugt13m_qtr, tmax_qtr)

var_uprop_yng_3m_ave=VARCALCUL(data_ult3m_yng_qtr, tmax_qtr)
var_uprop_yng_5m_ave=VARCALCUL(data_ult5m_yng_qtr, tmax_qtr)
var_uprop_yng_9m_ave=VARCALCUL(data_ult9m_yng_qtr, tmax_qtr)
var_uprop_yng_13m_ave=VARCALCUL(data_ult13m_yng_qtr, tmax_qtr)
var_uprop_yng_g13m_ave=VARCALCUL(data_ugt13m_yng_qtr, tmax_qtr)

var_l_uprop_yng_3m_ave=VARCALCUL(data_l_ult3m_yng_qtr, tmax_qtr)
var_l_uprop_yng_5m_ave=VARCALCUL(data_l_ult5m_yng_qtr, tmax_qtr)
var_l_uprop_yng_9m_ave=VARCALCUL(data_l_ult9m_yng_qtr, tmax_qtr)
var_l_uprop_yng_13m_ave=VARCALCUL(data_l_ult13m_yng_qtr, tmax_qtr)
var_l_uprop_yng_g13m_ave=VARCALCUL(data_l_ugt13m_yng_qtr, tmax_qtr)

var_uprop_prm_3m_ave=VARCALCUL(data_ult3m_prm_qtr, tmax_qtr)
var_uprop_prm_5m_ave=VARCALCUL(data_ult5m_prm_qtr, tmax_qtr)
var_uprop_prm_9m_ave=VARCALCUL(data_ult9m_prm_qtr, tmax_qtr)
var_uprop_prm_13m_ave=VARCALCUL(data_ult13m_prm_qtr, tmax_qtr)
var_uprop_prm_g13m_ave=VARCALCUL(data_ugt13m_prm_qtr, tmax_qtr)

var_l_uprop_prm_3m_ave=VARCALCUL(data_l_ult3m_prm_qtr, tmax_qtr)
var_l_uprop_prm_5m_ave=VARCALCUL(data_l_ult5m_prm_qtr, tmax_qtr)
var_l_uprop_prm_9m_ave=VARCALCUL(data_l_ult9m_prm_qtr, tmax_qtr)
var_l_uprop_prm_13m_ave=VARCALCUL(data_l_ult13m_prm_qtr, tmax_qtr)
var_l_uprop_prm_g13m_ave=VARCALCUL(data_l_ugt13m_prm_qtr, tmax_qtr)




var_ult5wk_ave=VARCALCUL(data_ult5wk_qtr, tmax_qtr)
var_u5lt15wk_ave=VARCALCUL(data_u5lt15wk_qtr, tmax_qtr)
var_u15lt27wk_ave=VARCALCUL(data_u15lt27wk_qtr, tmax_qtr)
var_ugt27wk_ave=VARCALCUL(data_ugt27wk_qtr, tmax_qtr)
var_l_ult5wk_ave=VARCALCUL(data_l_ult5wk_qtr, tmax_qtr)
var_l_u5lt15wk_ave=VARCALCUL(data_l_u5lt15wk_qtr, tmax_qtr)
var_l_u15lt27wk_ave=VARCALCUL(data_l_u15lt27wk_qtr, tmax_qtr)
var_l_ugt27wk_ave=VARCALCUL(data_l_ugt27wk_qtr, tmax_qtr)


!$OMP CRITICAL
ALLOCATE(Xjf(tmax_qtr-2,2), Yjf(1:tmax_qtr-2,1))

Xjf(:,1)=1
Xjf(:,2)=0.0_8

!
! 1. EMPIRICAL ELASTICITY OF THE MATCHING FUNCTION: LINDT
Xjf(:,1)=1
Xjf(:,2)=LOG(data_theta_qtr(1: tmax_qtr-2))
Yjf(1:tmax_qtr-2,1)=LOG(data_jf_qtr(1:tmax_qtr-2))
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf, errmsg)
elmatching_lindt_ave=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'elmatching_lindt_ave'
END IF

! 2. ELASTICITY U w/ v
Xjf(:,1)=1
Xjf(:,2)=LOG(data_v_qtr(1:tmax_qtr-2))
Yjf(1:tmax_qtr-2,1)=LOG(data_u_qtr(1: tmax_qtr-2))
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf, errmsg)
el_u_with_v_lindt=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_u_with_v_lind'
END IF
! 3. ELASTICITY U w/ P
Xjf(:,1)=1
Xjf(:,2)=LOG(data_prod_qtr(1:tmax_qtr-2))
Yjf(1:tmax_qtr-2,1)=LOG(data_u_qtr(1: tmax_qtr-2))
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf, errmsg)
el_u_with_p_lindt=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_u_with_p_lindt'
END IF
! 4. UNEMPLOYMENT DURATION
Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_occ_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf, errmsg)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_with_u'
END IF
udur_occ_with_u=betajf(2)

Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_young_occ_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
udur_occ_young_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_young_with_u'
END IF

Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_prime_occ_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
udur_occ_prime_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_prime_with_u'
END IF

Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_nocc_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
udur_nocc_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_with_u'
END IF

Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_young_nocc_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
udur_nocc_young_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_young_with_u'
END IF

Xjf(:,1)=1
Xjf(:,2)=data_u_qtr(1: tmax_qtr-2)
Yjf(1:tmax_qtr-2,1)=data_ucompldur_prime_nocc_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
udur_nocc_prime_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_prime_with_u'
END IF

! 5. DURATION DISTRIBUTION WITH UNEMPLOYMENT
Xjf(:,2)=LOG(data_u_qtr(1: tmax_qtr-2))
Yjf(1:tmax_qtr-2,1)=data_l_ult5wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5w_unemp_elasticity'
END IF

Yjf(1:tmax_qtr-2,1)=data_l_u5lt15wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u15w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u15w_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_u15lt27wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u27w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u27w_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ugt27wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ugt27w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ugt27w_unemp_elasticity'
END IF



Yjf(1:tmax_qtr-2,1)=data_l_ult3m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult5m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult9m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult13m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ugt13m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_elasticity'
    END IF


!! YOUNG


    Yjf(1:tmax_qtr-2,1)=data_l_ult3m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult5m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult9m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult13m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ugt13m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_yng_elasticity'
    END IF

!! PRIME
    Yjf(1:tmax_qtr-2,1)=data_l_ult3m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult5m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult9m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ult13m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_l_ugt13m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_prm_elasticity'
END IF




Yjf(1:tmax_qtr-2,1)=data_ult5wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_u5lt15wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u15w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u15w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_u15lt27wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u27w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u27w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ugt27wk_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ugt27w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ugt27w_unemp_semielasticity'
    END IF


!! ALL WORKERS

Yjf(1:tmax_qtr-2,1)=data_ult3m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult5m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult9m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult13m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ugt13m_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_semielasticity'
    END IF

!! YOUNG WORKERS


Yjf(1:tmax_qtr-2,1)=data_ult3m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult5m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult9m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult13m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ugt13m_yng_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_yng_semielasticity'
    END IF


!! PRIME-AGED WORKERS


Yjf(1:tmax_qtr-2,1)=data_ult3m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u3m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u3m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult5m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u5m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u5m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult9m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u9m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u9m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ult13m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
u13m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u13m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-2,1)=data_ugt13m_prm_qtr(1:tmax_qtr-2)
CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
ug13m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'ug13m_unemp_prm_semielasticity'
END IF


!!==========================================
!! BEVERIDGE CurVE LINEARLY DETRENDED!!!!!
!!==========================================

tempreal=0.0_8      ! covariance
tempreal2=0.0_8     ! mean u
tempreal3=0.0_8     ! mean v
tempreal4=0.0_8     ! variance u
tempreal5=0.0_8     ! variance v


tempint=0

DO t=1, tmax_qtr-2
     tempreal=tempreal+(LOG(data_u_qtr(t))*LOG(data_v_qtr(t)))
     tempreal2=tempreal2+LOG(data_u_qtr(t))
     tempreal3=tempreal3+LOG(data_v_qtr(t))
     tempreal4=tempreal4+(LOG(data_u_qtr(t))*LOG(data_u_qtr(t)))
     tempreal5=tempreal5+(LOG(data_v_qtr(t))*LOG(data_v_qtr(t)))
     tempint=tempint+1
END DO

corr_uv_lindt=(tempreal/REAL(tempint))-(tempreal2/REAL(tempint))*(tempreal3/REAL(tempint))
corr_uv_lindt=corr_uv_lindt/(sqrt((tempreal4/REAL(tempint)))*sqrt((tempreal5/REAL(tempint))))



DEALLOCATE(Xjf, Yjf)
!$OMP END CRITICAL


!===============================================================
!          MONTHLY TIME SERIES, written to file
!===============================================================

!IF (threadnumber==1) THEN
!OPEN(UNIT=93, file="monthly_time_series2.txt", status="replace", form="formatted")
!IF (matrix_statistics==1) WRITE(93, FMT='(13(A6))', ADVANCE='NO') "time,", "agg p,", "u,", "v,", "theta,", "sep,", "jf,", "wage,", "prod,", "reall,", "and simul:,"
!WRITE(93, FMT='(12(A6))') "time,", "agg p,", "u,", "v,", "theta,", "sep,", "jf,", "wage,", "prod,", "reall,"
!DO t=1,tmax_sim2
!IF (matrix_statistics==1) WRITE(93, FMT='(I6, A1, 9(F7.5, A1))') t, ",", pvector(aggprod_ind(t)), ",", data_u_qtr(t), "," , data_v_qtr(t), "," ,data_theta_qtr(t), ",", &
!        data_sep_qtr(t), ","  , data_jf_qtr(t), ","  , data_wage_qtr(t), ","  , data_prod_qtr(t), ","  , data_reall_qtr(t)
!WRITE(93, FMT='(I6, A1, 3(F10.7, X, A1, X))', ADVANCE='NO') t, ",", pvector(aggprod_ind(t)), ",", data_u_qtr(t), "," , data_v_qtr(t), ","
!WRITE(93, FMT='(6(F10.7, X, A1, X))') data_theta_qtr(t), ",", data_sep_qtr(t), ","  , data_jf_qtr(t), ","  , data_wage_qtr(t), "," &
!                             , data_prod_qtr(t), ","  , data_reall_qtr(t)
!END DO
!CLOSE(93)
!END IF !(threadnumber==1)
!


!============================================
!          AGGREGATE TIME SERIES  -- LOG, QUARTERLY & HP-FILTERED
!============================================

!$OMP CRITICAL
tcnt=tmax_qtr/4
ALLOCATE(data_ulev_qtr(tmax_qtr), data_lsep_qtr(tmax_qtr), data_lsep_y_qtr(tmax_qtr), data_lsep_p_qtr(tmax_qtr), data_ljf_qtr(tmax_qtr), &
data_ljf_young_qtr(tmax_qtr), data_ljf_prime_qtr(tmax_qtr), data_lpocc_qtr(tmax_qtr), data_lpnocc_qtr(tmax_qtr), &
data_ljf5_qtr(tmax_qtr), data_ljf5_young_qtr(tmax_qtr), data_ljf5_prime_qtr(tmax_qtr), data_lpocc5_qtr(tmax_qtr), &
data_lpnocc5_qtr(tmax_qtr), data_temp3_qtr(tmax_qtr), data_lv5_qtr(tmax_qtr), data_lu5_qtr(tmax_qtr), data_luall5_qtr(tmax_qtr), data_ltheta5_qtr(tmax_qtr), &
data_lsep5_qtr(tmax_qtr), data_lprod5_qtr(tmax_qtr), data_lwage5_qtr(tmax_qtr), &
data_lcocc_outflow5_qtr(tmax_qtr), data_lcocc_young_outflow5_qtr(tmax_qtr), data_lcocc_prime_outflow5_qtr(tmax_qtr), &
data_u_yr(tcnt+1), data_sep_y_yr(tcnt+1), data_sep_p_yr(tcnt+1), data_jf_y_yr(tcnt+1), data_jf_p_yr(tcnt+1), data_u_y_yr(tcnt+1), data_u_p_yr(tcnt+1), &
data_sm5_ult3m_qtr(tmax_qtr), data_sm5_ult5m_qtr(tmax_qtr), &
data_sm5_ult9m_qtr(tmax_qtr), data_sm5_ult13m_qtr(tmax_qtr), data_sm5_ugt13m_qtr(tmax_qtr) &
)
!$OMP END CRITICAL


! LOG YEARLY SERIES: U, SEP_Y, SEP_P
        ! aggregate into yearly stats
    DO tcnt=1, tmax_qtr, 4
           data_u_yr(((tcnt-1)/4)+1)=0.25_8*(data_uall_qtr(tcnt)+ data_uall_qtr(tcnt+1)+data_uall_qtr(tcnt+2)+ data_uall_qtr(tcnt+3))
           data_sep_y_yr(((tcnt-1)/4)+1)=0.25_8*(data_sep_y_qtr(tcnt)+ data_sep_y_qtr(tcnt+1)+data_sep_y_qtr(tcnt+2)+ data_sep_y_qtr(tcnt+3))
           data_sep_p_yr(((tcnt-1)/4)+1)=0.25_8*(data_sep_p_qtr(tcnt)+ data_sep_p_qtr(tcnt+1)+data_sep_p_qtr(tcnt+2)+ data_sep_p_qtr(tcnt+3))
           data_jf_y_yr(((tcnt-1)/4)+1)=0.25_8*(data_jf_young_qtr(tcnt)+ data_jf_young_qtr(tcnt+1)+data_jf_young_qtr(tcnt+2)+ data_jf_young_qtr(tcnt+3))
           data_jf_p_yr(((tcnt-1)/4)+1)=0.25_8*(data_jf_prime_qtr(tcnt)+ data_jf_prime_qtr(tcnt+1)+data_jf_prime_qtr(tcnt+2)+ data_jf_prime_qtr(tcnt+3))
           data_u_y_yr(((tcnt-1)/4)+1)=0.25_8*(data_u_young_qtr(tcnt)+ data_u_young_qtr(tcnt+1)+data_u_young_qtr(tcnt+2)+ data_u_young_qtr(tcnt+3))
           data_u_p_yr(((tcnt-1)/4)+1)=0.25_8*(data_u_prime_qtr(tcnt)+ data_u_prime_qtr(tcnt+1)+data_u_prime_qtr(tcnt+2)+ data_u_prime_qtr(tcnt+3))
    END DO
    !! check
        uall_yrave=sum(data_u_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        sep_y_yrave=sum(data_sep_y_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        sep_p_yrave=sum(data_sep_p_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        jf_y_yrave=sum(data_jf_y_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        jf_p_yrave=sum(data_jf_p_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        u_y_yrave=sum(data_u_y_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)
        u_p_yrave=sum(data_u_p_yr(1:(tmax_qtr/4)))/REAL(tmax_qtr/4)




        ! log 'em
            data_u_yr(1:(tmax_qtr/4))=LOG(data_u_yr(1:(tmax_qtr/4)))
            data_sep_y_yr(1:(tmax_qtr/4))=LOG(data_sep_y_yr(1:(tmax_qtr/4)))
            data_sep_p_yr(1:(tmax_qtr/4))=LOG(data_sep_p_yr(1:(tmax_qtr/4)))
            data_jf_y_yr(1:(tmax_qtr/4))=LOG(data_jf_y_yr(1:(tmax_qtr/4)))
            data_jf_p_yr(1:(tmax_qtr/4))=LOG(data_jf_p_yr(1:(tmax_qtr/4)))
            data_u_y_yr(1:(tmax_qtr/4))=LOG(data_u_y_yr(1:(tmax_qtr/4)))
            data_u_p_yr(1:(tmax_qtr/4))=LOG(data_u_p_yr(1:(tmax_qtr/4)))


! LOG THE SERIES
        data_uraw_qtr(1:tmax_qtr)=data_u_qtr(1:tmax_qtr)
        data_ulev_qtr(1:tmax_qtr)=data_u_qtr(1:tmax_qtr)
        !data_reall_qtr(:)=LOG(data_reall_qtr(:))



        !data_ult5wk_qtr(1:tmax_qtr)=LOG(data_ult5wk_qtr(1:tmax_qtr))
        !data_u5lt15wk_qtr(1:tmax_qtr)=LOG(data_u5lt15wk_qtr(1:tmax_qtr))
        !data_u15lt27wk_qtr(1:tmax_qtr)=LOG(data_u15lt27wk_qtr(1:tmax_qtr))
        !data_ugt27wk_qtr(1:tmax_qtr)=LOG(data_ugt27wk_qtr(1:tmax_qtr))

! initialize with original series
    data_ljf5_qtr(1:tmax_qtr)=LOG(data_jf_qtr(1:tmax_qtr))
    data_ljf5_young_qtr(1:tmax_qtr)=LOG(data_jf_young_qtr(1:tmax_qtr))
    data_ljf5_prime_qtr(1:tmax_qtr)=LOG(data_jf_prime_qtr(1:tmax_qtr))
    data_lpocc5_qtr(1:tmax_qtr)=LOG(data_pocc_qtr(1:tmax_qtr))
    data_lpnocc5_qtr(1:tmax_qtr)=LOG(data_pnocc_qtr(1:tmax_qtr))
    data_lv5_qtr(1:tmax_qtr)=LOG(data_v_qtr(1:tmax_qtr))
    data_lu5_qtr(1:tmax_qtr)=LOG(data_u_qtr(1:tmax_qtr))
    data_luall5_qtr(1:tmax_qtr)=LOG(data_uall_qtr(1:tmax_qtr)) 
    data_ltheta5_qtr(1:tmax_qtr)=LOG(data_theta_qtr(1:tmax_qtr))
    data_lprod5_qtr(1:tmax_qtr)=LOG(data_prod_qtr(1:tmax_qtr))
    data_lwage5_qtr(1:tmax_qtr)=LOG(data_wage_qtr(1:tmax_qtr))
    data_lsep5_qtr(1:tmax_qtr)=LOG(data_sep_qtr(1:tmax_qtr))
    !data_cocc_outflow_qtr(1:tmax_qtr)=LOG(data_cocc_outflow_qtr(1:tmax_qtr))
    !data_cocc_young_outflow_qtr(1:tmax_qtr)=LOG(data_cocc_young_outflow_qtr(1:tmax_qtr))
    !data_cocc_prime_outflow_qtr(1:tmax_qtr)=LOG(data_cocc_prime_outflow_qtr(1:tmax_qtr))

! smooth and take logs
DO tcnt=3, tmax_qtr-2
    data_ljf5_qtr(tcnt)=LOG(sum(data_jf_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_ljf5_young_qtr(tcnt)=LOG(sum(data_jf_young_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_ljf5_prime_qtr(tcnt)=LOG(sum(data_jf_prime_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lpocc5_qtr(tcnt)=LOG(sum(data_pocc_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lpnocc5_qtr(tcnt)=LOG(sum(data_pnocc_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lv5_qtr(tcnt)=LOG(sum(data_v_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lu5_qtr(tcnt)=LOG(sum(data_u_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_luall5_qtr(tcnt)=LOG(sum(data_uall_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_ltheta5_qtr(tcnt)=LOG(sum(data_theta_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lprod5_qtr(tcnt)=LOG(sum(data_prod_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lwage5_qtr(tcnt)=LOG(sum(data_wage_qtr((tcnt-2):(tcnt+2)))/5.0_8)
    data_lsep5_qtr(tcnt)=LOG(sum(data_sep_qtr((tcnt-2):(tcnt+2)))/5.0_8)


    END DO

data_sm5_ult3m_qtr=data_ult3m_qtr
data_sm5_ult5m_qtr=data_ult5m_qtr
data_sm5_ult9m_qtr=data_ult9m_qtr
data_sm5_ult13m_qtr=data_ult13m_qtr
data_sm5_ugt13m_qtr=data_ugt13m_qtr

DO t=3,tmax_qtr-2
    data_sm5_ult3m_qtr(t)=(data_sm5_ult3m_qtr(t-2)+data_sm5_ult3m_qtr(t-1)+data_sm5_ult3m_qtr(t)+data_sm5_ult3m_qtr(t+1)+data_sm5_ult3m_qtr(t+2))/5.0
    data_sm5_ult5m_qtr(t)=(data_sm5_ult5m_qtr(t-2)+data_sm5_ult5m_qtr(t-1)+data_sm5_ult5m_qtr(t)+data_sm5_ult5m_qtr(t+1)+data_sm5_ult5m_qtr(t+2))/5.0
    data_sm5_ult9m_qtr(t)=(data_sm5_ult9m_qtr(t-2)+data_sm5_ult9m_qtr(t-1)+data_sm5_ult9m_qtr(t)+data_sm5_ult9m_qtr(t+1)+data_sm5_ult9m_qtr(t+2))/5.0
    data_sm5_ult13m_qtr(t)=(data_sm5_ult13m_qtr(t-2)+data_sm5_ult13m_qtr(t-1)+data_sm5_ult13m_qtr(t)+data_sm5_ult13m_qtr(t+1)+data_sm5_ult13m_qtr(t+2))/5.0
    data_sm5_ugt13m_qtr(t)=(data_sm5_ugt13m_qtr(t-2)+data_sm5_ugt13m_qtr(t-1)+data_sm5_ugt13m_qtr(t)+data_sm5_ugt13m_qtr(t+1)+data_sm5_ugt13m_qtr(t+2))/5.0
END DO

!data_temp3_qtr=data_cocc_outflow_qtr
!data_lcocc_outflow5_qtr=0.0_8
!DO tcnt=3, tmax_qtr-2
!    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
!    END DO
!
!data_temp3_qtr=data_cocc_young_outflow_qtr
!data_lcocc_young_outflow5_qtr=0.0_8
!DO tcnt=3, tmax_qtr-2
!    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_young_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
!END DO
!
!
!data_temp3_qtr=data_cocc_prime_outflow_qtr
!data_lcocc_prime_outflow5_qtr=0.0_8
!DO tcnt=3, tmax_qtr-2
!    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_prime_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
!END DO


data_temp3_qtr=data_cocc_outflow_qtr
DO tcnt=3, tmax_qtr-2
    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
END DO

data_temp3_qtr=data_cocc_young_outflow_qtr
DO tcnt=3, tmax_qtr-2
    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_young_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
END DO

data_temp3_qtr=data_cocc_prime_outflow_qtr
DO tcnt=3, tmax_qtr-2
    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_lcocc_prime_outflow5_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
END DO


        data_u_qtr(1:tmax_qtr)=LOG(data_u_qtr(1:tmax_qtr))
        data_uall_qtr(1:tmax_qtr)=LOG(data_uall_qtr(1:tmax_qtr))
        data_v_qtr(1:tmax_qtr)=LOG(data_v_qtr(1:tmax_qtr))
        data_theta_qtr(1:tmax_qtr)=LOG(data_theta_qtr(1:tmax_qtr))
        data_lsep_qtr(1:tmax_qtr)=LOG(data_sep_qtr(1:tmax_qtr))
        data_ljf_qtr(1:tmax_qtr)=LOG(data_jf_qtr(1:tmax_qtr))

        data_lpocc_qtr(1:tmax_qtr)=LOG(data_pocc_qtr(1:tmax_qtr))
        data_lpnocc_qtr(1:tmax_qtr)=LOG(data_pnocc_qtr(1:tmax_qtr))


        data_wage_qtr(1:tmax_qtr)=LOG(data_wage_qtr(1:tmax_qtr))
        data_prod_qtr(1:tmax_qtr)=LOG(data_prod_qtr(1:tmax_qtr))

        data_u_young_qtr(1:tmax_qtr)=LOG(data_u_young_qtr(1:tmax_qtr))
        data_u_prime_qtr(1:tmax_qtr)=LOG(data_u_prime_qtr(1:tmax_qtr))

        data_ljf_young_qtr(1:tmax_qtr)=LOG(data_jf_young_qtr(1:tmax_qtr))
        data_ljf_prime_qtr(1:tmax_qtr)=LOG(data_jf_prime_qtr(1:tmax_qtr))

        data_lsep_y_qtr(1:tmax_qtr)=LOG(data_sep_y_qtr(1:tmax_qtr))
        data_lsep_p_qtr(1:tmax_qtr)=LOG(data_sep_p_qtr(1:tmax_qtr))

        data_cocc_inflow_qtr(1:tmax_qtr)=LOG(data_cocc_inflow_qtr(1:tmax_qtr))
        data_cocc_young_inflow_qtr(1:tmax_qtr)=LOG(data_cocc_young_inflow_qtr(1:tmax_qtr))
        data_cocc_prime_inflow_qtr(1:tmax_qtr)=LOG(data_cocc_prime_inflow_qtr(1:tmax_qtr))

        data_cocc_outflow_qtr=LOG(data_cocc_outflow_qtr)
        data_cocc_young_outflow_qtr=LOG(data_cocc_young_outflow_qtr)
        data_cocc_prime_outflow_qtr=LOG(data_cocc_prime_outflow_qtr)

        !
        !  OPEN(UNIT=21, file="original_quarterly_timeseries_sep.txt", status="replace", form="formatted")
        !WRITE(21,*) 'data_lsep5_qtr(tcnt), data_lsep_qtr(tcnt), data_sep_qtr(tcnt)'
        !DO tcnt=1, tmax_qtr
        !    WRITE(21,*) data_lsep5_qtr(tcnt), data_lsep_qtr(tcnt), data_sep_qtr(tcnt)
        !END DO
        !CLOSE(21)
        !OPEN(UNIT=20, file="original_quarterly_timeseries_prod.txt", status="replace", form="formatted")
        !WRITE(20,*) 'data_lprod5_qtr(tcnt), data_lsep_qtr(tcnt), data_sep_qtr(tcnt)'
        !DO tcnt=1, tmax_qtr
        !    WRITE(20,*) data_lprod5_qtr(tcnt), data_lprod_qtr(tcnt), data_prod_qtr(tcnt)
        !END DO
        !CLOSE(20)
        !OPEN(UNIT=24, file="original_quarterly_timeseries_reall.txt", status="replace", form="formatted")
        !WRITE(24,*) 'data_lcocc_outflow5_qtr, data_cocc_outflow_qtr'
        !DO tcnt=1, tmax_qtr
        !    WRITE(24,*) data_lcocc_outflow5_qtr, data_cocc_outflow_qtr
        !END DO
        !CLOSE(24)

data_temp3_qtr=data_pocc_qtr
DO tcnt=3, tmax_qtr-2
    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_pocc_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
END DO

data_temp3_qtr=data_pnocc_qtr
DO tcnt=3, tmax_qtr-2
    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_pnocc_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
END DO

!data_temp3_qtr=data_jf_qtr
!DO tcnt=3, tmax_qtr-2
!    IF(sum(data_temp3_qtr(tcnt-2:tcnt+2))>0.0) data_jf_qtr(tcnt)=LOG(sum(data_temp3_qtr(tcnt-2:tcnt+2))/5.0_8)
!END DO

!$OMP CRITICAL
DEALLOCATE(data_temp3_qtr)
!$OMP END CRITICAL

 !!$OMP MASTER
 !IF (verboseswitchfile==1) THEN
 !    OPEN(UNIT=21, file="quarterly_timeseries.txt", status="replace", form="formatted")
 !    WRITE(21,*) 'HP FILTERED ------------------------->'
 !    WRITE(21, FMT='(4(A15))', ADVANCE='NO') "time,", "hp_l_u,", "hp_l_v,", "hp_l_theta,"
 !    WRITE(21, FMT='(5(A15))', ADVANCE='NO') "hp_l_sep,", "hp_l_jf,", "hp_l_wage,", "hp_l_prod,", "hp_l_reall,"
 !    WRITE(21, FMT='(3(A15))', ADVANCE='NO') "log_u,", "log_v,", "log_theta,"
 !    WRITE(21, FMT='(5(A15))', ADVANCE='NO') "log_sep,", "log_jf,", "log_wage,", "log_prod,", "log_reall,"
 !    WRITE(21, FMT='(2(A15))') "log_jfocc,", "hp_l_jfocc,"
 !    DO t=1,tmax_qtr-1
 !        WRITE(21, FMT='(I5,9X, A1, 3(F14.6, A1))', ADVANCE='NO') t, "," , data_u_qtr(t), ",", data_v_qtr(t), ",", data_theta_qtr(t), ","
 !        WRITE(21, FMT='(5(F14.6, A1))', ADVANCE='NO') data_lsep_qtr(t), ",", data_ljf_qtr(t), ",", 0.0_8, ",", data_prod_qtr(t), ",", data_cocc_inflow_qtr(t), ","
 !        WRITE(21, FMT='(1(A2))') ',-'
 !    END DO
 !    CLOSE(21)
 !END IF
 !!$OMP END MASTER

! HP FILTER IT

! prescott's HP Filter code
!C ----------------------------------------------------------------------
!C  SR: hpfilt
!C  Kalman smoothing routine for HP filter written by E Prescott.
!C   y=data series, d=deviations from trend, t=trend, n=no. obs,
!C   s=smoothing parameter (eg, 1600 for std HP).
!C   Array v is scratch area and must have dimension at least 3n.
!C   If IOPT=1 and n and s are the same as for the previous call,
!C   the numbers in v are not recomputed.  This reduces execution
!C   time by about 30 percent.  Note that if this option is exercised,
!C   v cannot be used for other purposes between calls.
!C   This version does NOT release the trend in order to save memory.
!C ----------------------------------------------------------------------

!$OMP CRITICAL
     ALLOCATE(hp_scratch(4*(counter1),3), data_temp_qtr(counter1))


    counter1=tmax_qtr-2

     !     SUBROUTINE HPFILT(Y,D,V,N,S,IOPT)
    CALL HPFILT(data_ulev_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ulev_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_u_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_u_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_uall_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_uall_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_v_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_v_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_theta_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_theta_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_sep_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sep_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_lsep_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_lsep_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_jf_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_jf_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_ljf_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ljf_qtr(1:counter1)=data_temp_qtr(1:counter1)

!    CALL HPFILT(data_wage_qtr,data_temp_qtr,hp_scratch,tmax_qtr-1,hpfilterfactor,0)
!    data_u_qtr=data_temp_qtr

    CALL HPFILT(data_prod_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_prod_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_wage_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_wage_qtr(1:counter1)=data_temp_qtr(1:counter1)


!    CALL HPFILT(data_reall_qtr,data_temp_qtr,hp_scratch,tmax_qtr-1,hpfilterfactor,0)
!    data_u_qtr=data_temp_qtr


     !     SUBROUTINE HPFILT(Y,D,V,N,S,IOPT)
        ! young unemployment
    CALL HPFILT(data_u_young_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_u_young_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime unemployment
    CALL HPFILT(data_u_prime_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_u_prime_qtr(1:counter1)=data_temp_qtr(1:counter1)
            ! young job finding
    CALL HPFILT(data_jf_young_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_jf_young_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime job finding
    CALL HPFILT(data_jf_prime_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_jf_prime_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! young log job finding
    CALL HPFILT(data_jf_young_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_jf_young_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime log job finding
    CALL HPFILT(data_ljf_prime_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ljf_prime_qtr(1:counter1)=data_temp_qtr(1:counter1)

        ! young separation
    CALL HPFILT(data_sep_y_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sep_y_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime separation
    CALL HPFILT(data_sep_p_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sep_p_qtr(1:counter1)=data_temp_qtr(1:counter1)
            ! young separation
    CALL HPFILT(data_lsep_y_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_lsep_y_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime separation
    CALL HPFILT(data_lsep_p_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_lsep_p_qtr(1:counter1)=data_temp_qtr(1:counter1)

    CALL HPFILT(data_cocc_inflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_inflow_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! young reallocation composition
    CALL HPFILT(data_cocc_young_inflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_young_inflow_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime reallocation composition
    CALL HPFILT(data_cocc_prime_inflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_prime_inflow_qtr(1:counter1)=data_temp_qtr(1:counter1)


    CALL HPFILT(data_cocc_outflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_outflow_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! young reallocation composition
    CALL HPFILT(data_cocc_young_outflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_young_outflow_qtr(1:counter1)=data_temp_qtr(1:counter1)
        ! prime reallocation composition
    CALL HPFILT(data_cocc_prime_outflow_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_cocc_prime_outflow_qtr(1:counter1)=data_temp_qtr(1:counter1)


    !jfocc
    CALL HPFILT(data_pocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_pocc_qtr(1:counter1)=data_temp_qtr(1:counter1)
    !jfnocc
    CALL HPFILT(data_pnocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_pnocc_qtr(1:counter1)=data_temp_qtr(1:counter1)
    !log jfocc
    CALL HPFILT(data_lpocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_lpocc_qtr(1:counter1)=data_temp_qtr(1:counter1)
    !log jfnocc
    CALL HPFILT(data_lpnocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_lpnocc_qtr(1:counter1)=data_temp_qtr(1:counter1)

    ! PROPORTIONS UNEMPLOYMENT POOL: BLS
    ! U LESS THAN 5 WKS
    CALL HPFILT(data_ult5wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult5wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5 and 15
    CALL HPFILT(data_u5lt15wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_u5lt15wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 15 and 27
    CALL HPFILT(data_u15lt27wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_u15lt27wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5 and 15
    CALL HPFILT(data_ugt27wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ugt27wk_qtr(1:counter1)=data_temp_qtr(1:counter1)


    ! PROPORTIONS UNEMPLOYMENT POOL: BLS LOGGED
    ! U LESS THAN 5 WKS
    CALL HPFILT(data_l_ult5wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult5wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5 and 15
    CALL HPFILT(data_l_u5lt15wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_u5lt15wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 15 and 27
    CALL HPFILT(data_l_u15lt27wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_u15lt27wk_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5 and 15
    CALL HPFILT(data_l_ugt27wk_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ugt27wk_qtr(1:counter1)=data_temp_qtr(1:counter1)

    ! PROPORTIONS UNEMPLOYMENT POOL: SIPP
    !!-- ALL
    ! U LESS THAN 3 months
    CALL HPFILT(data_ult3m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult3m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 1-4
    CALL HPFILT(data_ult5m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult5m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-9
    CALL HPFILT(data_ult9m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult9m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12
    CALL HPFILT(data_ult13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult13m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 13-18
    CALL HPFILT(data_ugt13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ugt13m_qtr(1:counter1)=data_temp_qtr(1:counter1)

    ! SMOOTHED VERSION
    CALL HPFILT(data_sm5_ult3m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sm5_ult3m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 1-4
    CALL HPFILT(data_sm5_ult5m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sm5_ult5m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-9
    CALL HPFILT(data_sm5_ult9m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sm5_ult9m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12
    CALL HPFILT(data_sm5_ult13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sm5_ult13m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 13-18
    CALL HPFILT(data_sm5_ugt13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_sm5_ugt13m_qtr(1:counter1)=data_temp_qtr(1:counter1)

    ! LOGGED PROPORTIONS UNEMPLOYMENT POOL: SIPP LOG
    ! U LESS 3 MONTHS
    CALL HPFILT(data_l_ult3m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult3m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN <5m
    CALL HPFILT(data_l_ult5m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult5m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-8
    CALL HPFILT(data_l_ult9m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult9m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12m
    CALL HPFILT(data_l_ult13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult13m_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U 13-18m
    CALL HPFILT(data_l_ugt13m_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ugt13m_qtr(1:counter1)=data_temp_qtr(1:counter1)

    !!-- YNG
    ! U LESS THAN 3 months
    CALL HPFILT(data_ult3m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult3m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 1-4
    CALL HPFILT(data_ult5m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult5m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-9
    CALL HPFILT(data_ult9m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult9m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12
    CALL HPFILT(data_ult13m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult13m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 13-18
    CALL HPFILT(data_ugt13m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ugt13m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)


    ! LOGGED PROPORTIONS UNEMPLOYMENT POOL: SIPP LOG
    ! U LESS 3 MONTHS
    CALL HPFILT(data_l_ult3m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult3m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN <5m
    CALL HPFILT(data_l_ult5m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult5m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-8
    CALL HPFILT(data_l_ult9m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult9m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12m
    CALL HPFILT(data_l_ult13m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult13m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U 13-18m
    CALL HPFILT(data_l_ugt13m_yng_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ugt13m_yng_qtr(1:counter1)=data_temp_qtr(1:counter1)

    !!-- PRIME
    ! U LESS THAN 3 months
    CALL HPFILT(data_ult3m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult3m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 1-4
    CALL HPFILT(data_ult5m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult5m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-9
    CALL HPFILT(data_ult9m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult9m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12
    CALL HPFILT(data_ult13m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ult13m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 13-18
    CALL HPFILT(data_ugt13m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ugt13m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)


    ! LOGGED PROPORTIONS UNEMPLOYMENT POOL: SIPP LOG
    ! U LESS 3 MONTHS
    CALL HPFILT(data_l_ult3m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult3m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN <5m
    CALL HPFILT(data_l_ult5m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult5m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 5-8
    CALL HPFILT(data_l_ult9m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult9m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U BETWEEN 9-12m
    CALL HPFILT(data_l_ult13m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ult13m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)
    ! U 13-18m
    CALL HPFILT(data_l_ugt13m_prm_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_l_ugt13m_prm_qtr(1:counter1)=data_temp_qtr(1:counter1)




    ! smoothed job finding/u/v/theta/prod/wage/sep/cocc_outflow series


                ! CAREFUL WITH THE INDICES: COUNTER1 seems to go to e.g. 1160, but the size of the vector in is 4 less
    CALL HPFILT(data_ljf5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_ljf5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_ljf5_young_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_ljf5_young_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_ljf5_prime_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_ljf5_prime_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lpocc5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lpocc5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lpnocc5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lpnocc5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lv5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lv5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lu5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lu5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_luall5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_luall5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_ltheta5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_ltheta5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))

    CALL HPFILT(data_lsep5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lsep5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
        !OPEN(UNIT=23, file="hp_quarterly_timeseries.txt", status="replace", form="formatted")
        !WRITE(23,*) 'data_lsep5_qtr(tcnt), data_lsep_qtr(tcnt), data_sep_qtr(tcnt)'
        !DO tcnt=1, counter1
        !    WRITE(23,*) data_lsep5_qtr(tcnt), data_lsep_qtr(tcnt), data_sep_qtr(tcnt)
        !END DO
        !CLOSE(23)

    CALL HPFILT(data_lwage5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lwage5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lprod5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lprod5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))

    CALL HPFILT(data_lcocc_outflow5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lcocc_outflow5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lcocc_young_outflow5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lcocc_young_outflow5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))
    CALL HPFILT(data_lcocc_prime_outflow5_qtr(3:(counter1-2)),data_temp_qtr,hp_scratch,counter1-4,hpfilterfactor,0)
    data_lcocc_prime_outflow5_qtr(3:(counter1-2))=data_temp_qtr(3:(counter1-2))


    ! duration
    CALL HPFILT(data_ucompldur_occ_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_occ_qtr(1:counter1)=data_temp_qtr(1:counter1)
   CALL HPFILT(data_ucompldur_young_occ_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_young_occ_qtr(1:counter1)=data_temp_qtr(1:counter1)
   CALL HPFILT(data_ucompldur_prime_occ_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_prime_occ_qtr(1:counter1)=data_temp_qtr(1:counter1)
   CALL HPFILT(data_ucompldur_nocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_nocc_qtr(1:counter1)=data_temp_qtr(1:counter1)
   CALL HPFILT(data_ucompldur_young_nocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_young_nocc_qtr(1:counter1)=data_temp_qtr(1:counter1)
   CALL HPFILT(data_ucompldur_prime_nocc_qtr(1:counter1),data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    data_ucompldur_prime_nocc_qtr(1:counter1)=data_temp_qtr(1:counter1)


    ! YEARLY SERIES, ANDY MUELLER MOMENTS, HP FILTER with FACTOR 100

   CALL HPFILT(data_u_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_u_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_sep_y_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_sep_y_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_sep_p_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_sep_p_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_jf_y_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_jf_y_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_jf_p_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_jf_p_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_u_y_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_u_y_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
   CALL HPFILT(data_u_p_yr(1:(counter1/4)),data_temp_qtr,hp_scratch,(counter1/4),100.0_8,0)
    data_u_p_yr(1:(counter1/4))=data_temp_qtr(1:(counter1/4))
    DEALLOCATE(hp_scratch, data_temp_qtr)
!$OMP END CRITICAL



!============================================================
! top ISOLATE TOP/BOTTOM 33%, BOTTOM 50%, exclude ends of HP filtered time series
!============================================================

hpu_p33=quantile((33*counter1)/100, data_u_qtr(13:counter1-12))
hpu_p50=quantile((50*counter1)/100, data_u_qtr(13:counter1-12))
hpu_p67=quantile((67*counter1)/100, data_u_qtr(13:counter1-12))

!IF(verboseswitch==1) WRITE(*,*) 'HP U percentile cutoffs: p33, p50, p67', hpu_p33, hpu_p50, hpu_p67
IF(verbose_results_l0==1) WRITE(*,*) 'HP U percentile cutoffs: p33, p50, p67', hpu_p33, hpu_p50, hpu_p67
data_u_qtr_p33=0.0_8
data_u_qtr_p50=0.0_8
data_u_qtr_p67=0.0_8

DO pcnt=13,counter1-12
    IF(data_u_qtr(pcnt)<=hpu_p33) data_u_qtr_p33(pcnt)=1.0_8
    IF(data_u_qtr(pcnt)<=hpu_p50) data_u_qtr_p50(pcnt)=1.0_8
    IF(data_u_qtr(pcnt)>hpu_p67) data_u_qtr_p67(pcnt)=1.0_8
    END DO


            ! Calculate innerproduct
                    !result = MATMUL (matrix_a,matrix_b); If matrix_a has shape (m) and matrix_b has shape (m, k), the result is a rank-one array with shape (k)
!WRITE(*,*) data_totduration_cocc_qtr(counter1/2,:)
!WRITE(*,*) 'tot_durationmatrix_cocc', tot_durationmatrix_cocc



!-----------
! ALL, UNCORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cocc_p50(zcnt)=tot_durationmatrix_cocc_p50(zcnt)+data_totduration_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_p50(zcnt)=tot_durationmatrix_nocc_p50(zcnt)+data_totduration_nocc_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<=hpu_p33 ) THEN
            tot_durationmatrix_cocc_p33(zcnt)=tot_durationmatrix_cocc_p33(zcnt)+data_totduration_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_p33(zcnt)=tot_durationmatrix_nocc_p33(zcnt)+data_totduration_nocc_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cocc_p67(zcnt)=tot_durationmatrix_cocc_p67(zcnt)+data_totduration_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_p67(zcnt)=tot_durationmatrix_nocc_p67(zcnt)+data_totduration_nocc_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=REAL(tot_durationmatrix_cocc_p67(zcnt)+tot_durationmatrix_nocc_p67(zcnt))
IF(tempreal>0.0_8) tot_durationmatrix_cocc_p67(zcnt)=tot_durationmatrix_cocc_p67(zcnt)/tempreal
tempreal=REAL(tot_durationmatrix_cocc_p50(zcnt)+tot_durationmatrix_nocc_p50(zcnt))
IF(tempreal>0.0_8) tot_durationmatrix_cocc_p50(zcnt)=tot_durationmatrix_cocc_p50(zcnt)/tempreal
tempreal=REAL(tot_durationmatrix_cocc_p33(zcnt)+tot_durationmatrix_nocc_p33(zcnt))
IF(tempreal>0.0_8) tot_durationmatrix_cocc_p33(zcnt)=tot_durationmatrix_cocc_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_nocc_p67(1))
    tempreal2=REAL(tot_durationmatrix_nocc_p50(1))
    tempreal3=REAL(tot_durationmatrix_nocc_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_nocc_p67(zcnt)=tot_durationmatrix_nocc_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_nocc_p50(zcnt)=tot_durationmatrix_nocc_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_nocc_p33(zcnt)=tot_durationmatrix_nocc_p33(zcnt)/tempreal3
END IF

    END DO  ! zcnt loop over incomplete unemployment duration

! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_nocc_p67(1)=1.0_8
!tot_durationmatrix_nocc_p50(1)=1.0_8
!tot_durationmatrix_nocc_p33(1)=1.0_8

!-----------
! ALL, CORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cr_cocc_p50(zcnt)=tot_durationmatrix_cr_cocc_p50(zcnt)+data_totduration_cr_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_p50(zcnt)=tot_durationmatrix_cr_nocc_p50(zcnt)+data_totduration_cr_nocc_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<hpu_p33 ) THEN
            tot_durationmatrix_cr_cocc_p33(zcnt)=tot_durationmatrix_cr_cocc_p33(zcnt)+data_totduration_cr_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_p33(zcnt)=tot_durationmatrix_cr_nocc_p33(zcnt)+data_totduration_cr_nocc_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cr_cocc_p67(zcnt)=tot_durationmatrix_cr_cocc_p67(zcnt)+data_totduration_cr_cocc_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_p67(zcnt)=tot_durationmatrix_cr_nocc_p67(zcnt)+data_totduration_cr_nocc_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=tot_durationmatrix_cr_cocc_p67(zcnt)+tot_durationmatrix_cr_nocc_p67(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_p67(zcnt)=tot_durationmatrix_cr_cocc_p67(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_p50(zcnt)+tot_durationmatrix_cr_nocc_p50(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_p50(zcnt)=tot_durationmatrix_cr_cocc_p50(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_p33(zcnt)+tot_durationmatrix_cr_nocc_p33(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_p33(zcnt)=tot_durationmatrix_cr_cocc_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_cr_nocc_p67(1))
    tempreal2=REAL(tot_durationmatrix_cr_nocc_p50(1))
    tempreal3=REAL(tot_durationmatrix_cr_nocc_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_cr_nocc_p67(zcnt)=tot_durationmatrix_cr_nocc_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_cr_nocc_p50(zcnt)=tot_durationmatrix_cr_nocc_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_cr_nocc_p33(zcnt)=tot_durationmatrix_cr_nocc_p33(zcnt)/tempreal3
END IF
    END DO
! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_cr_nocc_p67(1)=1.0_8
!tot_durationmatrix_cr_nocc_p50(1)=1.0_8
!tot_durationmatrix_cr_nocc_p33(1)=1.0_8





!-----------
! YOUNG, UNCORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cocc_young_p50(zcnt)=tot_durationmatrix_cocc_young_p50(zcnt)+data_totduration_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_young_p50(zcnt)=tot_durationmatrix_nocc_young_p50(zcnt)+data_totduration_nocc_young_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<hpu_p33 ) THEN
            tot_durationmatrix_cocc_young_p33(zcnt)=tot_durationmatrix_cocc_young_p33(zcnt)+data_totduration_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_young_p33(zcnt)=tot_durationmatrix_nocc_young_p33(zcnt)+data_totduration_nocc_young_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cocc_young_p67(zcnt)=tot_durationmatrix_cocc_young_p67(zcnt)+data_totduration_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_young_p67(zcnt)=tot_durationmatrix_nocc_young_p67(zcnt)+data_totduration_nocc_young_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=tot_durationmatrix_cocc_young_p67(zcnt)+tot_durationmatrix_nocc_young_p67(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_young_p67(zcnt)=tot_durationmatrix_cocc_young_p67(zcnt)/tempreal
tempreal=tot_durationmatrix_cocc_young_p50(zcnt)+tot_durationmatrix_nocc_young_p50(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_young_p50(zcnt)=tot_durationmatrix_cocc_young_p50(zcnt)/tempreal
tempreal=tot_durationmatrix_cocc_young_p33(zcnt)+tot_durationmatrix_nocc_young_p33(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_young_p33(zcnt)=tot_durationmatrix_cocc_young_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_nocc_young_p67(1))
    tempreal2=REAL(tot_durationmatrix_nocc_young_p50(1))
    tempreal3=REAL(tot_durationmatrix_nocc_young_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_nocc_young_p67(zcnt)=tot_durationmatrix_nocc_young_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_nocc_young_p50(zcnt)=tot_durationmatrix_nocc_young_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_nocc_young_p33(zcnt)=tot_durationmatrix_nocc_young_p33(zcnt)/tempreal3
END IF
    END DO
! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_nocc_young_p67(1)=1.0_8
!tot_durationmatrix_nocc_young_p50(1)=1.0_8
!tot_durationmatrix_nocc_young_p33(1)=1.0_8

!-----------
! YOUNG, CORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cr_cocc_young_p50(zcnt)=tot_durationmatrix_cr_cocc_young_p50(zcnt)+data_totduration_cr_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_young_p50(zcnt)=tot_durationmatrix_cr_nocc_young_p50(zcnt)+data_totduration_cr_nocc_young_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<hpu_p33 ) THEN
            tot_durationmatrix_cr_cocc_young_p33(zcnt)=tot_durationmatrix_cr_cocc_young_p33(zcnt)+data_totduration_cr_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_young_p33(zcnt)=tot_durationmatrix_cr_nocc_young_p33(zcnt)+data_totduration_cr_nocc_young_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cr_cocc_young_p67(zcnt)=tot_durationmatrix_cr_cocc_young_p67(zcnt)+data_totduration_cr_cocc_young_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_young_p67(zcnt)=tot_durationmatrix_cr_nocc_young_p67(zcnt)+data_totduration_cr_nocc_young_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=tot_durationmatrix_cr_cocc_young_p67(zcnt)+tot_durationmatrix_cr_nocc_young_p67(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_young_p67(zcnt)=tot_durationmatrix_cr_cocc_young_p67(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_young_p50(zcnt)+tot_durationmatrix_cr_nocc_young_p50(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_young_p50(zcnt)=tot_durationmatrix_cr_cocc_young_p50(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_young_p33(zcnt)+tot_durationmatrix_cr_nocc_young_p33(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_young_p33(zcnt)=tot_durationmatrix_cr_cocc_young_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_cr_nocc_young_p67(1))
    tempreal2=REAL(tot_durationmatrix_cr_nocc_young_p50(1))
    tempreal3=REAL(tot_durationmatrix_cr_nocc_young_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_cr_nocc_young_p67(zcnt)=tot_durationmatrix_cr_nocc_young_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_cr_nocc_young_p50(zcnt)=tot_durationmatrix_cr_nocc_young_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_cr_nocc_young_p33(zcnt)=tot_durationmatrix_cr_nocc_young_p33(zcnt)/tempreal3
END IF
    END DO
! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_cr_nocc_young_p67(1)=1.0_8
!tot_durationmatrix_cr_nocc_young_p50(1)=1.0_8
!tot_durationmatrix_cr_nocc_young_p33(1)=1.0_8




!-----------
! PRIME, UNCORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cocc_prime_p50(zcnt)=tot_durationmatrix_cocc_prime_p50(zcnt)+data_totduration_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_prime_p50(zcnt)=tot_durationmatrix_nocc_prime_p50(zcnt)+data_totduration_nocc_prime_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<hpu_p33 ) THEN
            tot_durationmatrix_cocc_prime_p33(zcnt)=tot_durationmatrix_cocc_prime_p33(zcnt)+data_totduration_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_prime_p33(zcnt)=tot_durationmatrix_nocc_prime_p33(zcnt)+data_totduration_nocc_prime_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cocc_prime_p67(zcnt)=tot_durationmatrix_cocc_prime_p67(zcnt)+data_totduration_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_nocc_prime_p67(zcnt)=tot_durationmatrix_nocc_prime_p67(zcnt)+data_totduration_nocc_prime_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=tot_durationmatrix_cocc_prime_p67(zcnt)+tot_durationmatrix_nocc_prime_p67(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_prime_p67(zcnt)=tot_durationmatrix_cocc_prime_p67(zcnt)/tempreal
tempreal=tot_durationmatrix_cocc_prime_p50(zcnt)+tot_durationmatrix_nocc_prime_p50(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_prime_p50(zcnt)=tot_durationmatrix_cocc_prime_p50(zcnt)/tempreal
tempreal=tot_durationmatrix_cocc_prime_p33(zcnt)+tot_durationmatrix_nocc_prime_p33(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cocc_prime_p33(zcnt)=tot_durationmatrix_cocc_prime_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_nocc_prime_p67(1))
    tempreal2=REAL(tot_durationmatrix_nocc_prime_p50(1))
    tempreal3=REAL(tot_durationmatrix_nocc_prime_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_nocc_prime_p67(zcnt)=tot_durationmatrix_nocc_prime_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_nocc_prime_p50(zcnt)=tot_durationmatrix_nocc_prime_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_nocc_prime_p33(zcnt)=tot_durationmatrix_nocc_prime_p33(zcnt)/tempreal3
END IF
    END DO
! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_nocc_prime_p67(1)=1.0_8
!tot_durationmatrix_nocc_prime_p50(1)=1.0_8
!tot_durationmatrix_nocc_prime_p33(1)=1.0_8

!-----------
! PRIME, CORRECTED
!-----------
DO zcnt=1, 18
z2cnt=0
p2cnt=0
x2cnt=0
DO pcnt=1, counter1

     IF(data_u_qtr(pcnt)<=hpu_p50 ) THEN
            tot_durationmatrix_cr_cocc_prime_p50(zcnt)=tot_durationmatrix_cr_cocc_prime_p50(zcnt)+data_totduration_cr_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_prime_p50(zcnt)=tot_durationmatrix_cr_nocc_prime_p50(zcnt)+data_totduration_cr_nocc_prime_qtr(pcnt,zcnt)
            x2cnt=x2cnt+1
     END IF

     IF(data_u_qtr(pcnt)<hpu_p33 ) THEN
            tot_durationmatrix_cr_cocc_prime_p33(zcnt)=tot_durationmatrix_cr_cocc_prime_p33(zcnt)+data_totduration_cr_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_prime_p33(zcnt)=tot_durationmatrix_cr_nocc_prime_p33(zcnt)+data_totduration_cr_nocc_prime_qtr(pcnt,zcnt)
            p2cnt=p2cnt+1
     END IF


    IF (data_u_qtr(pcnt)>=hpu_p67) THEN
        tot_durationmatrix_cr_cocc_prime_p67(zcnt)=tot_durationmatrix_cr_cocc_prime_p67(zcnt)+data_totduration_cr_cocc_prime_qtr(pcnt,zcnt)
            tot_durationmatrix_cr_nocc_prime_p67(zcnt)=tot_durationmatrix_cr_nocc_prime_p67(zcnt)+data_totduration_cr_nocc_prime_qtr(pcnt,zcnt)
            z2cnt=z2cnt+1
    END IF

END DO
tempreal=tot_durationmatrix_cr_cocc_prime_p67(zcnt)+tot_durationmatrix_cr_nocc_prime_p67(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_prime_p67(zcnt)=tot_durationmatrix_cr_cocc_prime_p67(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_prime_p50(zcnt)+tot_durationmatrix_cr_nocc_prime_p50(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_prime_p50(zcnt)=tot_durationmatrix_cr_cocc_prime_p50(zcnt)/tempreal
tempreal=tot_durationmatrix_cr_cocc_prime_p33(zcnt)+tot_durationmatrix_cr_nocc_prime_p33(zcnt)
IF(tempreal>0.0_8) tot_durationmatrix_cr_cocc_prime_p33(zcnt)=tot_durationmatrix_cr_cocc_prime_p33(zcnt)/tempreal

IF(zcnt>1) THEN
    tempreal=REAL(tot_durationmatrix_cr_nocc_prime_p67(1))
    tempreal2=REAL(tot_durationmatrix_cr_nocc_prime_p50(1))
    tempreal3=REAL(tot_durationmatrix_cr_nocc_prime_p33(1))
    IF(tempreal>0.0_8) tot_durationmatrix_cr_nocc_prime_p67(zcnt)=tot_durationmatrix_cr_nocc_prime_p67(zcnt)/tempreal
    IF(tempreal2>0.0_8) tot_durationmatrix_cr_nocc_prime_p50(zcnt)=tot_durationmatrix_cr_nocc_prime_p50(zcnt)/tempreal2
    IF(tempreal3>0.0_8) tot_durationmatrix_cr_nocc_prime_p33(zcnt)=tot_durationmatrix_cr_nocc_prime_p33(zcnt)/tempreal3
END IF
    END DO
! FIRST ELEMENT: either normalize to zero, or keep as the total number of observations?
!tot_durationmatrix_cr_nocc_prime_p67(1)=1.0_8
!tot_durationmatrix_cr_nocc_prime_p50(1)=1.0_8
!tot_durationmatrix_cr_nocc_prime_p33(1)=1.0_8


            !tot_durationmatrix_nocc_p33/50/67 should collect the number of spells observed

!WRITE(*,*) '------------------------------'
!WRITE(*,*) 'tot_durationmatrix_cocc_p33'
!WRITE(*,*) tot_durationmatrix_cocc_p33

!WRITE(*,*)  'tot_durationmatrix_cocc_p67'
!WRITE(*,*)  tot_durationmatrix_cocc_p67
!WRITE(*,*) '------------------------------'

profileshift_shortdur=0.0_8
tempreal=0.0_8
tempreal2=0.0_8
do tcnt=1,6
    tempreal=tempreal+tot_durationmatrix_cocc_p33(tcnt)
    tempreal2=tempreal2+tot_durationmatrix_cocc_p67(tcnt)
end do

IF (tempreal2>0.0_8) profileshift_shortdur=tempreal/tempreal2

profileshift_shortdur=0.0_8
tempreal=0.0_8
tempreal2=0.0_8
do tcnt=7,12
    tempreal=tempreal+tot_durationmatrix_cocc_p33(tcnt)
    IF(tcnt>9) tempreal2=tempreal2+tot_durationmatrix_cocc_p67(tcnt)
end do
    tempreal=tempreal/3.0_8
    tempreal2=tempreal2/6.0_8
IF (tempreal2>0.0_8) profileshift_longdur=tempreal/tempreal2

!==========================================================
!    WRITE QUARTERLY AGGREGATE TIME SERIES TO FILE
!=========================================================


 !!$OMP MASTER
 !INQUIRE(FILE="quarterly_timeseries.txt", EXIST=file_exists)
 !IF (verbosetimeseriesfile==1 .AND. file_exists) THEN
 !    OPEN(UNIT=21, file="quarterly_timeseries.txt", status="old", form="formatted", position='append')
 !    WRITE(21,*) 'HP FILTERED ------------------------->'
 !    WRITE(21,*) 'threadno: ', threadnumber
 !    WRITE(21, FMT='(4(A15))', ADVANCE='NO') "time,", "hp_l_u,", "hp_l_v,", "hp_l_theta,"
 !    WRITE(21, FMT='(5(A15))', ADVANCE='NO') "hp_l_sep,", "hp_l_jf,", "hp_l_wage,", "hp_l_prod,", "hp_l_reall,"
 !    WRITE(21, FMT='(3(A15))', ADVANCE='NO') "log_u,", "log_v,", "log_theta,"
 !    WRITE(21, FMT='(5(A15))', ADVANCE='NO') "log_sep,", "log_jf,", "log_wage,", "log_prod,", "log_reall,"
 !    WRITE(21, FMT='(2(A15))') "log_jfocc,", "hp_l_jfocc,"
 !
 !    DO t=1,tmax_qtr-2
 !        WRITE(21, FMT='(I5,9X, A1, 3(F14.6, A1))', ADVANCE='NO') t, "," , data_u_qtr(t), ",", data_v_qtr(t), ",", data_theta_qtr(t), ","
 !        WRITE(21, FMT='(5(F10.6, A1))', ADVANCE='NO') data_lsep_qtr(t), ",", data_ljf_qtr(t), ",", 0.0_8, ",", data_prod_qtr(t), ",", data_cocc_inflow_qtr(t), ","
 !        WRITE(21, *) ', -'
 !    END DO
 !    CLOSE(21)
 !END IF
 !!$OMP END MASTER




!=====================================================================================================
!============================================
!          AGGREGATE TIME SERIES  -- ELASTICITIES, AUTOCORRELATIONS, CORRELATIONS
!============================================
!=========================================================================================================

! INITIALIZE

var_u=0.0_8
var_v=0.0_8
var_theta=0.0_8
var_sep=0.0_8
var_jf=0.0_8
var_jfo=0.0_8
var_wage=0.0_8
var_prod=0.0_8
var_reall=0.0_8
var_u5=0.0_8
var_v5=0.0_8
var_theta5=0.0_8

var_sep5=0.0_8
var_jf5=0.0_8
var_jfo5=0.0_8
var_wage5=0.0_8
var_prod5=0.0_8
var_reall5=0.0_8
var_reall5_2=0.0_8

corr_uv=0.0_8
corr_uv_lindt=0.0_8
corr_utheta=0.0_8
corr_usep=0.0_8
corr_ujf=0.0_8
corr_uwage=0.0_8
corr_uprod=0.0_8
corr_ureall=0.0_8
corr_vtheta=0.0_8

corr_vsep=0.0_8
corr_vjf=0.0_8
corr_vwage=0.0_8
corr_vprd=0.0_8
corr_vreall=0.0_8

corr_thetasep=0.0_8
corr_thetajf=0.0_8
corr_thetawage=0.0_8
corr_thetaprod=0.0_8
corr_thetareall=0.0_8

corr_sepjf=0.0_8
corr_sepwage=0.0_8
corr_sepprod=0.0_8
corr_sepreall=0.0_8

corr_jfwage=0.0_8
corr_jfprod=0.0_8
corr_wageprod=0.0_8
corr_wagereall=0.0_8
corr_prodreall=0.0_8

corr_jfreall=0.0_8
corr_vprod=0.0_8
corr_ujfo=0.0_8
corr_vjfo=0.0_8
corr_thetajfo=0.0_8
corr_sepjfo=0.0_8
corr_jfjfo=0.0_8

corr_wagejfo=0.0_8
corr_prodjfo=0.0_8
corr_realljfo=0.0_8

corr_uv5=0.0_8
corr_uv5_2=0.0_8
corr_utheta5=0.0_8
corr_usep5=0.0_8
corr_ujf5=0.0_8
corr_uwage5=0.0_8
corr_uprod5=0.0_8
corr_ureall5=0.0_8
corr_vtheta5=0.0_8

corr_vsep5=0.0_8
corr_vjf5=0.0_8
corr_vwage5=0.0_8
corr_vprd5=0.0_8
corr_vreall5=0.0_8

corr_thetasep5=0.0_8
corr_thetajf5=0.0_8
corr_thetawage5=0.0_8
corr_thetaprod5=0.0_8
corr_thetareall5=0.0_8
corr_sepjf5=0.0_8
corr_sepwage5=0.0_8
corr_sepprod5=0.0_8
corr_sepreall5=0.0_8
corr_jfwage5=0.0_8
corr_jfprod5=0.0_8
corr_wageprod5=0.0_8
corr_wagereall5=0.0_8
corr_prodreall5=0.0_8

corr_jfreall5=0.0_8
corr_vprod5=0.0_8
corr_ujfo5=0.0_8
corr_vjfo5=0.0_8
corr_thetajfo5=0.0_8
corr_sepjfo5=0.0_8
corr_jfjfo5=0.0_8

corr_wagejfo5=0.0_8
corr_prodjfo5=0.0_8
corr_realljfo5=0.0_8

ac_u=0.0_8
ac_v=0.0_8
ac_theta=0.0_8
ac_sep=0.0_8
ac_jf=0.0_8
ac_wage=0.0_8
ac_prod=0.0_8
ac_reall=0.0_8
ac_jfo=0.0_8
ac_u5=0.0_8

ac_v5=0.0_8
ac_theta5=0.0_8
ac_sep5=0.0_8
ac_jf5=0.0_8
ac_wage5=0.0_8
ac_prod5=0.0_8
ac_reall5=0.0_8
ac_jfo5=0.0_8


jfocc5_prod_elasticity=0.0_8
jfnocc5_prod_elasticity=0.0_8
jf5_young_prod_elasticity=0.0_8
jf5_prime_prod_elasticity=0.0_8
jf5_prod_elasticity=0.0_8
jfocc5_prod_semielasticity=0.0_8
jfnocc5_prod_semielasticity=0.0_8
jf5_young_prod_semielasticity=0.0_8
jf5_prime_prod_semielasticity=0.0_8
jf5_prod_semielasticity=0.0_8
jfocc5_unemp_semielasticity=0.0_8
jfnocc5_unemp_semielasticity=0.0_8
jf5_young_unemp_semielasticity=0.0_8
jf5_prime_unemp_semielasticity=0.0_8
jf5_unemp_semielasticity=0.0_8
jfocc5_unemp_elasticity=0.0_8
jfnocc5_unemp_elasticity=0.0_8
jf5_young_unemp_elasticity=0.0_8
jf5_prime_unemp_elasticity=0.0_8
jf5_unemp_elasticity=0.0_8

jfocc_unemp_elasticity=0.0_8
jfnocc_unemp_elasticity=0.0_8
jf_young_unemp_elasticity=0.0_8
jf_prime_unemp_elasticity=0.0_8
jf_unemp_elasticity=0.0_8
sep_unemp_elasticity=0.0_8

sep_young_prod_semielasticity=0.0_8
sep_prod_semielasticity=0.0_8
sep_prime_prod_semielasticity=0.0_8
jfocc_prod_semielasticity=0.0_8
jfnocc_prod_semielasticity=0.0_8

jf_prod_semielasticity=0.0_8
jf_young_prod_semielasticity=0.0_8
jf_prime_prod_semielasticity=0.0_8

sep_young_unemp_semielasticity=0.0_8
sep_unemp_semielasticity=0.0_8
sep_prime_unemp_semielasticity=0.0_8
jfocc_unemp_semielasticity=0.0_8
jfnocc_unemp_semielasticity=0.0_8

jf_unemp_semielasticity=0.0_8
jf_young_unemp_semielasticity=0.0_8
jf_prime_unemp_semielasticity=0.0_8

hp_uprop3m_prod_elasticity=0.0_8
hp_uprop5m_prod_elasticity=0.0_8
hp_uprop9m_prod_elasticity=0.0_8
hp_uprop13m_prod_elasticity=0.0_8
hp_upropg13m_prod_elasticity=0.0_8
hp_uprop3m_prod_semielasticity=0.0_8
hp_uprop5m_prod_semielasticity=0.0_8
hp_uprop9m_prod_semielasticity=0.0_8
hp_uprop13m_prod_semielasticity=0.0_8
hp_upropg13m_prod_semielasticity=0.0_8
hp_uprop3m_unemp_elasticity=0.0_8
hp_uprop5m_unemp_elasticity=0.0_8
hp_uprop9m_unemp_elasticity=0.0_8
hp_uprop13m_unemp_elasticity=0.0_8
hp_upropg13m_unemp_elasticity=0.0_8
hp_sm_uprop3m_unemp_elasticity=0.0_8
hp_sm_uprop5m_unemp_elasticity=0.0_8
hp_sm_uprop9m_unemp_elasticity=0.0_8
hp_sm_uprop13m_unemp_elasticity=0.0_8
hp_sm_upropg13m_unemp_elasticity=0.0_8

hp_uprop3m_unemp_semielasticity=0.0_8
hp_uprop5m_unemp_semielasticity=0.0_8
hp_uprop9m_unemp_semielasticity=0.0_8
hp_uprop13m_unemp_semielasticity=0.0_8
hp_upropg13m_unemp_semielasticity=0.0_8
hp_uprop3m_prod_yng_elasticity=0.0_8
hp_uprop5m_prod_yng_elasticity=0.0_8
hp_uprop9m_prod_yng_elasticity=0.0_8
hp_uprop13m_prod_yng_elasticity=0.0_8
hp_upropg13m_prod_yng_elasticity=0.0_8

hp_uprop3m_prod_yng_semielasticity=0.0_8
hp_uprop5m_prod_yng_semielasticity=0.0_8
hp_uprop9m_prod_yng_semielasticity=0.0_8
hp_uprop13m_prod_yng_semielasticity=0.0_8
hp_upropg13m_prod_yng_semielasticity=0.0_8
hp_uprop3m_unemp_yng_elasticity=0.0_8
hp_uprop5m_unemp_yng_elasticity=0.0_8
hp_uprop9m_unemp_yng_elasticity=0.0_8
hp_uprop13m_unemp_yng_elasticity=0.0_8
hp_upropg13m_unemp_yng_elasticity=0.0_8
hp_uprop3m_unemp_yng_semielasticity=0.0_8
hp_uprop5m_unemp_yng_semielasticity=0.0_8
hp_uprop9m_unemp_yng_semielasticity=0.0_8
hp_uprop13m_unemp_yng_semielasticity=0.0_8
hp_upropg13m_unemp_yng_semielasticity=0.0_8
hp_uprop3m_prod_prm_elasticity=0.0_8
hp_uprop5m_prod_prm_elasticity=0.0_8
hp_uprop9m_prod_prm_elasticity=0.0_8
hp_uprop13m_prod_prm_elasticity=0.0_8
hp_upropg13m_prod_prm_elasticity=0.0_8
hp_uprop3m_prod_prm_semielasticity=0.0_8
hp_uprop5m_prod_prm_semielasticity=0.0_8
hp_uprop9m_prod_prm_semielasticity=0.0_8
hp_uprop13m_prod_prm_semielasticity=0.0_8
hp_upropg13m_prod_prm_semielasticity=0.0_8
hp_uprop3m_unemp_prm_elasticity=0.0_8
hp_uprop5m_unemp_prm_elasticity=0.0_8
hp_uprop9m_unemp_prm_elasticity=0.0_8
hp_uprop13m_unemp_prm_elasticity=0.0_8
hp_upropg13m_unemp_prm_elasticity=0.0_8
hp_uprop3m_unemp_prm_semielasticity=0.0_8
hp_uprop5m_unemp_prm_semielasticity=0.0_8
hp_uprop9m_unemp_prm_semielasticity=0.0_8
hp_uprop13m_unemp_prm_semielasticity=0.0_8
hp_upropg13m_unemp_prm_semielasticity=0.8

sm_hp_uprop3m_u_semi_el=0.0_8
sm_hp_uprop5m_u_semi_el=0.0_8
sm_hp_uprop9m_u_semi_el=0.0_8
sm_hp_uprop13m_u_semi_el=0.0_8
sm_hp_upropg13m_u_semi_el=0.0_8


! means quarterly: hp filtered: should equal 0 more or less
u_ave_q=sum(data_u_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
uall_ave_q=sum(data_uall_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
v_ave_q=sum(data_v_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
theta_ave_q=sum(data_theta_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
sep_ave_q=sum(data_lsep_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
jf_ave_q=sum(data_ljf_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
prod_ave_q=sum(data_prod_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
reall_ave_q=sum(data_cocc_outflow_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)


u_ave_q5=sum(data_lu5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
uall_ave_q5=sum(data_luall5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
v_ave_q5=sum(data_lv5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
theta_ave_q5=sum(data_ltheta5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
sep_ave_q5=sum(data_lsep5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
jf_ave_q5=sum(data_ljf5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
prod_ave_q5=sum(data_lprod5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)
reall_ave_q5=sum(data_lcocc_outflow5_qtr(13:tmax_qtr-12))/REAL(tmax_qtr-24.0)

!WRITE(*,*) 'HP DETRENDED AVERAGES, should be zero'
!WRITE(*,*) 'u_ave_q=', u_ave_q, 'v_ave_q=', v_ave_q, 'theta_ave_q=', theta_ave_q, 'sep_ave_q=', sep_ave_q
!WRITE(*,*) 'jf_ave_q=', jf_ave_q, 'wage_ave_q=', wage_ave_q, 'prod_ave_q=', prod_ave_q, 'reall_ave_q=', reall_ave_q

!=============================
! VARIANCES UNSMOOTHED
!================================


var_u=VARCALCUL(data_u_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_uall=VARCALCUL(data_uall_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_v=VARCALCUL(data_v_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_theta=VARCALCUL(data_theta_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_jf=VARCALCUL(data_ljf_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_sep=VARCALCUL(data_lsep_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_prod=VARCALCUL(data_prod_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_reall=VARCALCUL(data_cocc_outflow_qtr(13:tmax_qtr-12), tmax_qtr-24)

!!u
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+(data_u_qtr(t))**2.0_8
!     tempint=tempint+1
!END DO
!!WRITE(*,*) 'tempreal: sum of sq u=', tempreal
!!WRITE(*,*) 'tempint=', tempint
!var_u=tempreal/REAL(tempint)
!!WRITE(*,*) 'unconditional variance', var_u, 'and average u', u_ave_q
!var_u=(tempreal/REAL(tempint))-(u_ave_q)**2.0_8
!!WRITE(*,*) 'afterwards var_u=,', var_u
!
!!PAUSE
!
!
!
!!v
!
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_v=(tempreal/REAL(tempint))-(v_ave_q)**2.0_8
!!theta
!
!
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_theta_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_theta=(tempreal/REAL(tempint))-(theta_ave_q)**2.0_8
!
!! jf
!
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_ljf_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_jf=(tempreal/REAL(tempint))-(jf_ave_q)**2.0_8
!
!
!
!! sep
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_lsep_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_sep=(tempreal/REAL(tempint))-(sep_ave_q)**2.0_8
!
!
!!! wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_wage_qtr(t)**2.0_8
!!     tempint=tempint+1
!!END DO
!!var_wage=(tempreal/REAL(tempint))-(wage_ave_q)**2.0_8
!
!
!! prod
!
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_prod_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_prod=(tempreal/REAL(tempint))-(prod_ave_q)**2.0_8
!
!
!! reall
!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-2
!     tempreal=tempreal+data_cocc_outflow_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_reall=(tempreal/REAL(tempint))-(reall_ave_q)**2.0_8




!=============================
! VARIANCES **SMOOTHED**
!================================


var_u5=VARCALCUL(data_lu5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_uall5=VARCALCUL(data_luall5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_v5=VARCALCUL(data_lv5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_theta5=VARCALCUL(data_ltheta5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_jf5=VARCALCUL(data_ljf5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_sep5=VARCALCUL(data_lsep5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_prod5=VARCALCUL(data_lprod5_qtr(13:tmax_qtr-12), tmax_qtr-24)
var_reall5=VARCALCUL(data_lcocc_outflow5_qtr(13:tmax_qtr-12), tmax_qtr-24)

!
!!u
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+(data_lu5_qtr(t))**2.0_8
!     tempint=tempint+1
!END DO
!!WRITE(*,*) 'tempreal: sum of sq u=', tempreal
!!WRITE(*,*) 'tempint=', tempint
!var_u5=tempreal/REAL(tempint)
!!WRITE(*,*) 'unconditional variance', var_u5, 'and average u', u_ave_q5
!var_u5=(tempreal/REAL(tempint))-(u_ave_q5)**2.0_8
!!WRITE(*,*) 'afterwards var_u=,', var_u
!
!!PAUSE
!
!
!
!!v
!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_v5=(tempreal/REAL(tempint))-(v_ave_q5)**2.0_8
!!theta
!
!! theta
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ltheta5_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_theta5=(tempreal/REAL(tempint))-(theta_ave_q5)**2.0_8
!
!! jf
!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ljf5_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_jf5=(tempreal/REAL(tempint))-(jf_ave_q5)**2.0_8
!
!
!
!! sep
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lsep5_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_sep5=(tempreal/REAL(tempint))-(sep_ave_q5)**2.0_8
!
!
!!! wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_wage_qtr(t)**2.0_8
!!     tempint=tempint+1
!!END DO
!!var_wage5=(tempreal/REAL(tempint))-(wage_ave_q5)**2.0_8
!
!
!! prod
!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lprod5_qtr(t)**2.0_8
!     tempint=tempint+1
!END DO
!var_prod5=(tempreal/REAL(tempint))-(prod_ave_q5)**2.0_8
!
!
!! reall
!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-2
!     tempreal=tempreal+(data_lcocc_outflow5_qtr(t))**2.0_8
!     tempint=tempint+1
!END DO
!var_reall5=(tempreal/REAL(tempint))-(reall_ave_q5)**2.0_8
!
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-24
     tempreal=tempreal+(data_lcocc_outflow5_qtr(t))**2.0_8
     tempint=tempint+1
END DO
var_reall5_2=(tempreal/REAL(tempint))-(reall_ave_q5)**2.0_8


!===============================
! CORRELATION TABLE UNSMOOTHED
!===============================

! Correlations with u
!cov_uv, cov_utheta, cov_usep, cov_ujf, cov_uwage, cov_uprod, cov_ureall,

corr_uv=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_v_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_utheta=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_theta_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_usep=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_lsep_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ujf=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_ljf_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_uwage=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uprod=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ureall=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! Correlations with u
!cov_uallv, cov_ualltheta, cov_uallsep, cov_ualljf, cov_uallwage, cov_uallprod, cov_uallreall,

corr_uallv=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_v_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallu=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_u_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ualltheta=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_theta_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallsep=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_lsep_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ualljf=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_ljf_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_uallwage=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallprod=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallreall=CORRCALCUL(data_uall_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! correlations with v

corr_vtheta=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_theta_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vsep=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_lsep_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vjf=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_ljf_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_vwage=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vprod=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vreall=CORRCALCUL(data_v_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

! tightness


corr_thetasep=CORRCALCUL(data_theta_qtr(13:(tmax_qtr-12)), data_lsep_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetajf=CORRCALCUL(data_theta_qtr(13:(tmax_qtr-12)), data_ljf_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_thetawage=CORRCALCUL(data_theta_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetaprod=CORRCALCUL(data_theta_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetareall=CORRCALCUL(data_theta_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

! separations


corr_sepjf=CORRCALCUL(data_lsep_qtr(13:(tmax_qtr-12)), data_ljf_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_sepwage=CORRCALCUL(data_lsep_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_sepprod=CORRCALCUL(data_lsep_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_sepreall=CORRCALCUL(data_lsep_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! job finding

!corr_jfwage=CORRCALCUL(data_ljf_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_jfprod=CORRCALCUL(data_ljf_qtr(13:(tmax_qtr-12)), data_prod_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_jfreall=CORRCALCUL(data_ljf_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! productivity

!corr_prodwage=CORRCALCUL(data_u_qtr(13:(tmax_qtr-12)), data_wage_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_prodreall=CORRCALCUL(data_prod_qtr(13:(tmax_qtr-12)), data_cocc_outflow_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

!
!
!
!
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+(data_u_qtr(t)*data_v_qtr(t))
!     tempint=tempint+1
!END DO
!corr_uv=(tempreal/REAL(tempint))-(u_ave_q)*(v_ave_q)
!corr_uv=corr_uv/(sqrt(var_v)*sqrt(var_u))
!
!! u with theta
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_u_qtr(t)*data_theta_qtr(t)
!     tempint=tempint+1
!END DO
!corr_utheta=(tempreal/REAL(tempint))-(u_ave_q)*(theta_ave_q)
!corr_utheta=corr_utheta/(sqrt(var_u)*sqrt(var_theta))
!
!! u with jf
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_u_qtr(t)*data_ljf_qtr(t)
!     tempint=tempint+1
!END DO
!corr_ujf=(tempreal/REAL(tempint))-(u_ave_q)*(jf_ave_q)
!corr_ujf=corr_ujf/(sqrt(var_u)*sqrt(var_jf))
!
!
!! u with sep
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_u_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_usep=(tempreal/REAL(tempint))-(u_ave_q)*(sep_ave_q)
!corr_usep=corr_usep/(sqrt(var_u)*sqrt(var_sep))
!
!! u with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_u_qtr(t)*data_prod_qtr(t)
!     tempint=tempint+1
!END DO
!corr_uprod=(tempreal/REAL(tempint))-(u_ave_q)*(prod_ave_q)
!corr_uprod=corr_uprod/(sqrt(var_u)*sqrt(var_prod))
!
!! u with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_u_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_uwage=(tempreal/REAL(tempint))-(u_ave_q)*(wage_ave_q)
!!corr_uwage=corr_uwage/(sqrt(var_u)*sqrt(var_wage))
!
!! u with reall
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_u_qtr(t)*data_cocc_outflow_qtr(t)
!     tempint=tempint+1
!END DO
!corr_ureall=(tempreal/REAL(tempint))-(u_ave_q)*(reall_ave_q)
!corr_ureall=corr_ureall/(sqrt(var_u)*sqrt(var_reall))
!
!
!! CORRELATIONS WITH V
!!cov_vtheta, cov_vsep, cov_vjf, cov_vwage, cov_vprd, cov_vreall,
!
!! v with theta
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)*data_theta_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vtheta=(tempreal/REAL(tempint))-(v_ave_q)*(theta_ave_q)
!corr_vtheta=corr_vtheta/(sqrt(var_v)*sqrt(var_theta))
!
!! v with sep
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vsep=(tempreal/REAL(tempint))-(v_ave_q)*(sep_ave_q)
!corr_vsep=corr_vsep/(sqrt(var_v)*sqrt(var_sep))
!
!! v with jf
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)*data_ljf_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vjf=(tempreal/REAL(tempint))-(v_ave_q)*(jf_ave_q)
!corr_vjf=corr_vjf/(sqrt(var_v)*sqrt(var_jf))
!
!
!
!
!
!! v with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_v_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_vwage=(tempreal/REAL(tempint))-(v_ave_q)*(wage_ave_q)
!!corr_vwage=corr_vwage/(sqrt(var_v)*sqrt(var_wage))
!
!! v with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)*data_prod_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vprod=(tempreal/REAL(tempint))-(v_ave_q)*(prod_ave_q)
!corr_vprod=corr_vprod/(sqrt(var_v)*sqrt(var_prod))
!
!! v with reall
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_v_qtr(t)*data_cocc_outflow_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vreall=(tempreal/REAL(tempint))-(v_ave_q)*(reall_ave_q)
!corr_vreall=corr_vreall/(sqrt(var_v)*sqrt(var_reall))
!
!
!
!
!
!! CORRELATIONS WITH THETA
!!cov_thetasep, cov_thetajf, cov_thetawage, cov_thetaprod, cov_thetareall, &
!
!! sep with theta
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_theta_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetasep=(tempreal/REAL(tempint))-(theta_ave_q)*(sep_ave_q)
!corr_thetasep=corr_thetasep/(sqrt(var_theta)*sqrt(var_sep))
!
!
!
!! theta with jf
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_theta_qtr(t)*data_ljf_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetajf=(tempreal/REAL(tempint))-(theta_ave_q)*(jf_ave_q)
!corr_thetajf=corr_thetajf/(sqrt(var_theta)*sqrt(var_jf))
!
!
!
!! theta with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_theta_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_thetawage=(tempreal/REAL(tempint))-(theta_ave_q)*(wage_ave_q)
!!corr_thetawage=corr_thetawage/(sqrt(var_theta)*sqrt(var_wage))
!
!
!! theta with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_theta_qtr(t)*data_prod_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetaprod=(tempreal/REAL(tempint))-(theta_ave_q)*(prod_ave_q)
!corr_thetaprod=corr_thetaprod/(sqrt(var_theta)*sqrt(var_prod))
!
!
!! theta  with reall
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_theta_qtr(t)*data_cocc_outflow_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetareall=(tempreal/REAL(tempint))-(theta_ave_q)*(reall_ave_q)
!corr_thetareall=corr_thetareall/(sqrt(var_theta)*sqrt(var_reall))
!
!! CORRELATIONS WITH SEP
!! cov_sepjf, cov_sepwage, cov_sepprod, cov_sepreall, &
!
!
!! sep with jf
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_ljf_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepjf=(tempreal/REAL(tempint))-(jf_ave_q)*(sep_ave_q)
!corr_sepjf=corr_sepjf/(sqrt(var_jf)*sqrt(var_sep))
!
!
!
!
!! sep with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_wage_qtr(t)*data_sep_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_sepwage=(tempreal/REAL(tempint))-(wage_ave_q)*(sep_ave_q)
!!corr_sepwage=corr_sepwage/(sqrt(var_wage)*sqrt(var_sep))
!
!
!! sep with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_prod_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepprod=(tempreal/REAL(tempint))-(prod_ave_q)*(sep_ave_q)
!corr_sepprod=corr_sepprod/(sqrt(var_prod)*sqrt(var_sep))
!
!
!! sep with reall
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_cocc_outflow_qtr(t)*data_lsep_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepreall=(tempreal/REAL(tempint))-(reall_ave_q)*(sep_ave_q)
!corr_sepreall=corr_sepreall/(sqrt(var_reall)*sqrt(var_sep))
!
!! CORRELATIONS WITH JF
!! cov_jfwage, cov_jfprod, covjfreall
!
!
!! jf with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_wage_qtr(t)*data_ljf_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_jfwage=(tempreal/REAL(tempint))-(wage_ave_q)*(jf_ave_q)
!!corr_jfwage=corr_jfwage/(sqrt(var_wage)*sqrt(var_jf))
!
!! jf with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_prod_qtr(t)*data_ljf_qtr(t)
!     tempint=tempint+1
!END DO
!corr_jfprod=(tempreal/REAL(tempint))-(prod_ave_q)*(jf_ave_q)
!corr_jfprod=corr_jfprod/(sqrt(var_prod)*sqrt(var_jf))
!
!! jf with reall
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_cocc_outflow_qtr(t)*data_ljf_qtr(t)
!     tempint=tempint+1
!END DO
!corr_jfreall=(tempreal/REAL(tempint))-(reall_ave_q)*(jf_ave_q)
!corr_jfreall=corr_jfreall/(sqrt(var_reall)*sqrt(var_jf))
!
!
!
!
!
!
!! CORRELATIONS WITH WAGE
!!cov_wageprod, cov_wagereall,
!
!! prod with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_wage_qtr(t)*data_prod_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_wageprod=(tempreal/REAL(tempint))-(wage_ave_q)*(prod_ave_q)
!!corr_wageprod=corr_wageprod/(sqrt(var_wage)*sqrt(var_prod))
!
!!! reall with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=1, tmax_qtr-2
!!     tempreal=tempreal+data_wage_qtr(t)*data_cocc_outflow_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_wagereall=(tempreal/REAL(tempint))-(wage_ave_q)*(reall_ave_q)
!!corr_wagereall=corr_wagereall/(sqrt(var_wage)*sqrt(var_reall))
!
!
!! CORRELATIONS WITH PROD
!!covprod_reall
!
!! reall with prod
!tempreal=0.0_8
!tempint=0
!DO t=1, tmax_qtr-2
!     tempreal=tempreal+data_prod_qtr(t)*data_cocc_outflow_qtr(t)
!     tempint=tempint+1
!END DO
!corr_prodreall=(tempreal/REAL(tempint))-(prod_ave_q)*(reall_ave_q)
!corr_prodreall=corr_prodreall/(sqrt(var_prod)*sqrt(var_reall))

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~AUTOCORRELATIONS

!ac_u, ac_v, ac_theta, ac_sep, ac_jf, ac_wage, ac_prod, ac_reall
! ac_u
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_u_qtr(t)*data_u_qtr(t-1)
     tempint=tempint+1
END DO
ac_u=(tempreal/REAL(tempint))-(u_ave_q)*(u_ave_q)
ac_u=ac_u/var_u

! ac_uall
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_uall_qtr(t)*data_uall_qtr(t-1)
     tempint=tempint+1
END DO
ac_uall=(tempreal/REAL(tempint))-(uall_ave_q)*(uall_ave_q)
ac_uall=ac_uall/var_uall


! ac_v
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_v_qtr(t)*data_v_qtr(t-1)
     tempint=tempint+1
END DO
ac_v=(tempreal/REAL(tempint))-(v_ave_q)*(v_ave_q)
ac_v=ac_v/var_v


! ac_theta
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_theta_qtr(t)*data_theta_qtr(t-1)
     tempint=tempint+1
END DO
ac_theta=(tempreal/REAL(tempint))-(theta_ave_q)*(theta_ave_q)
ac_theta=ac_theta/var_theta


! ac_jf
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_ljf_qtr(t)*data_ljf_qtr(t-1)
     tempint=tempint+1
END DO
ac_jf=(tempreal/REAL(tempint))-(jf_ave_q)*(jf_ave_q)
ac_jf=ac_jf/var_jf




! ac_sep
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lsep_qtr(t)*data_lsep_qtr(t-1)
     tempint=tempint+1
END DO
ac_sep=(tempreal/REAL(tempint))-(sep_ave_q)*(sep_ave_q)
ac_sep=ac_sep/var_sep


! ac_prod
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_prod_qtr(t)*data_prod_qtr(t-1)
     tempint=tempint+1
END DO
ac_prod=(tempreal/REAL(tempint))-(prod_ave_q)*(prod_ave_q)
ac_prod=ac_prod/var_prod


!! ac_wage
!tempreal=0.0_8
!tempint=0
!DO t=13, tmax_qtr-12
!     tempreal=tempreal+data_wage_qtr(t)*data_wage_qtr(t-1)
!     tempint=tempint+1
!END DO
!ac_wage=(tempreal/REAL(tempint))-(wage_ave_q)*(wage_ave_q)
!ac_wage=ac_wage/var_wage


! ac_reall
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_cocc_outflow_qtr(t)*data_cocc_outflow_qtr(t-1)
     tempint=tempint+1
END DO
ac_reall=(tempreal/REAL(tempint))-(reall_ave_q)*(reall_ave_q)
ac_reall=ac_reall/var_reall



!===============================
! CORRELATION TABLE **SMOOTHED**
!===============================

! Correlations with u
!cov_uv, cov_utheta, cov_usep, cov_ujf, cov_uwage, cov_uprod, cov_ureall,

corr_uv5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_lv5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_utheta5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_ltheta5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_usep5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_lsep5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ujf5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_ljf5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_uwage5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uprod5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ureall5=CORRCALCUL(data_lu5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

! Correlations with uall
!cov_uv, cov_utheta, cov_usep, cov_ujf, cov_uwage, cov_uprod, cov_ureall,

corr_uallu5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_lu5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallv5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_lv5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ualltheta5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_ltheta5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallsep5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_lsep5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_ualljf5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_ljf5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_uallwage5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallprod5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_uallreall5=CORRCALCUL(data_luall5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! correlations with v

corr_vtheta5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_ltheta5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vsep5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_lsep5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vjf5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_ljf5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_vwage5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vprod5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_vreall5=CORRCALCUL(data_lv5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

! tightness


corr_thetasep5=CORRCALCUL(data_ltheta5_qtr(13:(tmax_qtr-12)), data_lsep5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetajf5=CORRCALCUL(data_ltheta5_qtr(13:(tmax_qtr-12)), data_ljf5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_thetawage5=CORRCALCUL(data_ltheta5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetaprod5=CORRCALCUL(data_ltheta5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_thetareall5=CORRCALCUL(data_ltheta5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

! separations


corr_sepjf5=CORRCALCUL(data_lsep5_qtr(13:(tmax_qtr-12)), data_ljf5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
!corr_sepwage5=CORRCALCUL(data_lsep5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_sepprod5=CORRCALCUL(data_lsep5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_sepreall5=CORRCALCUL(data_lsep5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! job finding

!corr_jfwage5=CORRCALCUL(data_ljf5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_jfprod5=CORRCALCUL(data_ljf5_qtr(13:(tmax_qtr-12)), data_lprod5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_jfreall5=CORRCALCUL(data_ljf5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)


! productivity

!corr_prodwage5=CORRCALCUL(data_u5_qtr(13:(tmax_qtr-12)), data_wage5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)
corr_prodreall5=CORRCALCUL(data_lprod5_qtr(13:(tmax_qtr-12)), data_lcocc_outflow5_qtr(13:(tmax_qtr-12)), tmax_qtr-24)

!
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+(data_lu5_qtr(t)*data_lv5_qtr(t))
!     tempint=tempint+1
!END DO
!corr_uv5=(tempreal/REAL(tempint))-(u_ave_q5)*(v_ave_q5)
!corr_uv5=corr_uv5/(sqrt(var_v5)*sqrt(var_u5))
!
!! u with theta
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lu5_qtr(t)*data_ltheta5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_utheta5=(tempreal/REAL(tempint))-(u_ave_q5)*(theta_ave_q5)
!corr_utheta5=corr_utheta5/(sqrt(var_u5)*sqrt(var_theta5))
!
!! u with jf
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lu5_qtr(t)*data_ljf5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_ujf5=(tempreal/REAL(tempint))-(u_ave_q5)*(jf_ave_q5)
!corr_ujf5=corr_ujf5/(sqrt(var_u5)*sqrt(var_jf5))
!
!
!! u with sep
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lu5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_usep5=(tempreal/REAL(tempint))-(u_ave_q5)*(sep_ave_q5)
!corr_usep5=corr_usep5/(sqrt(var_u5)*sqrt(var_sep5))
!
!! u with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lu5_qtr(t)*data_lprod5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_uprod5=(tempreal/REAL(tempint))-(u_ave_q5)*(prod_ave_q5)
!corr_uprod5=corr_uprod5/(sqrt(var_u5)*sqrt(var_prod5))
!
!! u with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_u_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_uwage=(tempreal/REAL(tempint))-(u_ave_q)*(wage_ave_q)
!!corr_uwage=corr_uwage/(sqrt(var_u)*sqrt(var_wage))
!
!! u with reall
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lu5_qtr(t)*data_lcocc_outflow5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_ureall5=(tempreal/REAL(tempint))-(u_ave_q5)*(reall_ave_q5)
!corr_ureall5=corr_ureall5/(sqrt(var_u5)*sqrt(var_reall5))
!
!
!!-------
!! CORRELATIONS WITH V
!!cov_vtheta, cov_vsep, cov_vjf, cov_vwage, cov_vprd, cov_vreall,
!
!! v with theta
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)*data_ltheta5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vtheta5=(tempreal/REAL(tempint))-(v_ave_q5)*(theta_ave_q5)
!corr_vtheta5=corr_vtheta5/(sqrt(var_v5)*sqrt(var_theta5))
!
!! v with sep
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vsep5=(tempreal/REAL(tempint))-(v_ave_q5)*(sep_ave_q5)
!corr_vsep5=corr_vsep5/(sqrt(var_v5)*sqrt(var_sep5))
!
!! v with jf
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)*data_ljf5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vjf5=(tempreal/REAL(tempint))-(v_ave_q5)*(jf_ave_q5)
!corr_vjf5=corr_vjf5/(sqrt(var_v5)*sqrt(var_jf5))
!
!
!
!
!
!! v with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_v_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_vwage=(tempreal/REAL(tempint))-(v_ave_q)*(wage_ave_q)
!!corr_vwage=corr_vwage/(sqrt(var_v)*sqrt(var_wage))
!
!! v with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)*data_lprod5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vprod5=(tempreal/REAL(tempint))-(v_ave_q5)*(prod_ave_q5)
!corr_vprod5=corr_vprod5/(sqrt(var_v5)*sqrt(var_prod5))
!
!! v with reall
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lv5_qtr(t)*data_lcocc_outflow5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_vreall5=(tempreal/REAL(tempint))-(v_ave_q5)*(reall_ave_q5)
!corr_vreall5=corr_vreall5/(sqrt(var_v5)*sqrt(var_reall5))
!
!
!
!
!
!! CORRELATIONS WITH THETA
!!cov_thetasep, cov_thetajf, cov_thetawage, cov_thetaprod, cov_thetareall, &
!
!! sep with theta
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ltheta5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetasep5=(tempreal/REAL(tempint))-(theta_ave_q5)*(sep_ave_q5)
!corr_thetasep5=corr_thetasep5/(sqrt(var_theta5)*sqrt(var_sep5))
!
!
!
!! theta with jf
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ltheta5_qtr(t)*data_ljf5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetajf5=(tempreal/REAL(tempint))-(theta_ave_q5)*(jf_ave_q5)
!corr_thetajf5=corr_thetajf5/(sqrt(var_theta5)*sqrt(var_jf5))
!
!
!
!! theta with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_theta_qtr(t)*data_wage_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_thetawage=(tempreal/REAL(tempint))-(theta_ave_q)*(wage_ave_q)
!!corr_thetawage=corr_thetawage/(sqrt(var_theta)*sqrt(var_wage))
!
!
!! theta with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ltheta5_qtr(t)*data_lprod5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetaprod5=(tempreal/REAL(tempint))-(theta_ave_q5)*(prod_ave_q5)
!corr_thetaprod5=corr_thetaprod5/(sqrt(var_theta5)*sqrt(var_prod5))
!
!
!! theta  with reall
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ltheta5_qtr(t)*data_lcocc_outflow5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_thetareall5=(tempreal/REAL(tempint))-(theta_ave_q5)*(reall_ave_q5)
!corr_thetareall5=corr_thetareall5/(sqrt(var_theta5)*sqrt(var_reall5))
!
!! CORRELATIONS WITH SEP
!! cov_sepjf, cov_sepwage, cov_sepprod, cov_sepreall, &
!
!
!! sep with jf
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_ljf5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepjf5=(tempreal/REAL(tempint))-(jf_ave_q5)*(sep_ave_q5)
!corr_sepjf5=corr_sepjf5/(sqrt(var_jf5)*sqrt(var_sep5))
!
!
!
!
!! sep with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_wage_qtr(t)*data_sep_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_sepwage=(tempreal/REAL(tempint))-(wage_ave_q)*(sep_ave_q)
!!corr_sepwage=corr_sepwage/(sqrt(var_wage)*sqrt(var_sep))
!
!
!! sep with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lprod5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepprod5=(tempreal/REAL(tempint))-(prod_ave_q5)*(sep_ave_q5)
!corr_sepprod5=corr_sepprod5/(sqrt(var_prod5)*sqrt(var_sep5))
!
!
!! sep with reall
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lcocc_outflow5_qtr(t)*data_lsep5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_sepreall5=(tempreal/REAL(tempint))-(reall_ave_q5)*(sep_ave_q5)
!corr_sepreall5=corr_sepreall5/(sqrt(var_reall5)*sqrt(var_sep5))
!
!! CORRELATIONS WITH JF
!! cov_jfwage, cov_jfprod, covjfreall
!
!
!! jf with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_wage_qtr(t)*data_ljf_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_jfwage=(tempreal/REAL(tempint))-(wage_ave_q)*(jf_ave_q)
!!corr_jfwage=corr_jfwage/(sqrt(var_wage)*sqrt(var_jf))
!
!! jf with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lprod5_qtr(t)*data_ljf5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_jfprod5=(tempreal/REAL(tempint))-(prod_ave_q5)*(jf_ave_q5)
!corr_jfprod5=corr_jfprod5/(sqrt(var_prod5)*sqrt(var_jf5))
!
!! jf with reall
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lcocc_outflow5_qtr(t)*data_ljf5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_jfreall5=(tempreal/REAL(tempint))-(reall_ave_q5)*(jf_ave_q5)
!corr_jfreall5=corr_jfreall5/(sqrt(var_reall5)*sqrt(var_jf5))
!
!
!
!
!
!
!! CORRELATIONS WITH WAGE
!!cov_wageprod, cov_wagereall,
!
!! prod with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_wage_qtr(t)*data_prod_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_wageprod=(tempreal/REAL(tempint))-(wage_ave_q)*(prod_ave_q)
!!corr_wageprod=corr_wageprod/(sqrt(var_wage)*sqrt(var_prod))
!
!!! reall with wage
!!tempreal=0.0_8
!!tempint=0
!!DO t=3, tmax_qtr-3
!!     tempreal=tempreal+data_wage_qtr(t)*data_cocc_outflow_qtr(t)
!!     tempint=tempint+1
!!END DO
!!corr_wagereall=(tempreal/REAL(tempint))-(wage_ave_q)*(reall_ave_q)
!!corr_wagereall=corr_wagereall/(sqrt(var_wage)*sqrt(var_reall))
!
!
!! CORRELATIONS WITH PROD
!!covprod_reall
!
!! reall with prod
!tempreal=0.0_8
!tempint=0
!DO t=3, tmax_qtr-3
!     tempreal=tempreal+data_lprod5_qtr(t)*data_lcocc_outflow5_qtr(t)
!     tempint=tempint+1
!END DO
!corr_prodreall5=(tempreal/REAL(tempint))-(prod_ave_q5)*(reall_ave_q5)
!corr_prodreall5=corr_prodreall5/(sqrt(var_prod5)*sqrt(var_reall5))
!


!================================================
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~AUTOCORRELATIONS: UNSMOOTHED
!=================================================

!ac_u, ac_v, ac_theta, ac_sep, ac_jf, ac_wage, ac_prod, ac_reall
! ac_u


tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_u_qtr(t)*data_u_qtr(t-1)
     tempint=tempint+1
END DO
ac_u=(tempreal/REAL(tempint))-(u_ave_q)*(u_ave_q)
ac_u=ac_u/var_u

! ac_v
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_v_qtr(t)*data_v_qtr(t-1)
     tempint=tempint+1
END DO
ac_v=(tempreal/REAL(tempint))-(v_ave_q)*(v_ave_q)
ac_v=ac_v/var_v


! ac_theta
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_theta_qtr(t)*data_theta_qtr(t-1)
     tempint=tempint+1
END DO
ac_theta=(tempreal/REAL(tempint))-(theta_ave_q)*(theta_ave_q)
ac_theta=ac_theta/var_theta


! ac_jf
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_ljf_qtr(t)*data_ljf_qtr(t-1)
     tempint=tempint+1
END DO
ac_jf=(tempreal/REAL(tempint))-(jf_ave_q)*(jf_ave_q)
ac_jf=ac_jf/var_jf




! ac_sep
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lsep_qtr(t)*data_lsep_qtr(t-1)
     tempint=tempint+1
END DO
ac_sep=(tempreal/REAL(tempint))-(sep_ave_q)*(sep_ave_q)
ac_sep=ac_sep/var_sep


! ac_prod
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_prod_qtr(t)*data_prod_qtr(t-1)
     tempint=tempint+1
END DO
ac_prod=(tempreal/REAL(tempint))-(prod_ave_q)*(prod_ave_q)
ac_prod=ac_prod/var_prod


!! ac_wage
!tempreal=0.0_8
!tempint=0
!DO t=13, tmax_qtr-12
!     tempreal=tempreal+data_wage_qtr(t)*data_wage_qtr(t-1)
!     tempint=tempint+1
!END DO
!ac_wage=(tempreal/REAL(tempint))-(wage_ave_q)*(wage_ave_q)
!ac_wage=ac_wage/var_wage


! ac_reall
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_cocc_outflow_qtr(t)*data_cocc_outflow_qtr(t-1)
     tempint=tempint+1
END DO
ac_reall=(tempreal/REAL(tempint))-(reall_ave_q)*(reall_ave_q)
ac_reall=ac_reall/var_reall



!================================================
! ~~~~AUTOCORRELATIONS: **SMOOTHED**
!=================================================

!ac_u, ac_v, ac_theta, ac_sep, ac_jf, ac_wage, ac_prod, ac_reall
! ac_u
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lu5_qtr(t)*data_lu5_qtr(t-1)
     tempint=tempint+1
END DO
ac_u5=(tempreal/REAL(tempint))-(u_ave_q5)*(u_ave_q5)
ac_u5=ac_u5/var_u5

! ac_uall
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_luall5_qtr(t)*data_luall5_qtr(t-1)
     tempint=tempint+1
END DO
ac_uall5=(tempreal/REAL(tempint))-(uall_ave_q5)*(uall_ave_q5)
ac_uall5=ac_uall5/var_uall5


! ac_v
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lv5_qtr(t)*data_lv5_qtr(t-1)
     tempint=tempint+1
END DO
ac_v5=(tempreal/REAL(tempint))-(v_ave_q5)*(v_ave_q5)
ac_v5=ac_v5/var_v5


! ac_theta
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_ltheta5_qtr(t)*data_ltheta5_qtr(t-1)
     tempint=tempint+1
END DO
ac_theta5=(tempreal/REAL(tempint))-(theta_ave_q5)*(theta_ave_q5)
ac_theta5=ac_theta5/var_theta5


! ac_jf
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_ljf5_qtr(t)*data_ljf5_qtr(t-1)
     tempint=tempint+1
END DO
ac_jf5=(tempreal/REAL(tempint))-(jf_ave_q5)*(jf_ave_q5)
ac_jf5=ac_jf5/var_jf5




! ac_sep
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lsep5_qtr(t)*data_lsep5_qtr(t-1)
     tempint=tempint+1
END DO
ac_sep5=(tempreal/REAL(tempint))-(sep_ave_q5)*(sep_ave_q5)
ac_sep5=ac_sep5/var_sep5


! ac_prod
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lprod5_qtr(t)*data_lprod5_qtr(t-1)
     tempint=tempint+1
END DO
ac_prod5=(tempreal/REAL(tempint))-(prod_ave_q5)*(prod_ave_q5)
ac_prod5=ac_prod5/var_prod5


!! ac_wage
!tempreal=0.0_8
!tempint=0
!DO t=13, tmax_qtr-12
!     tempreal=tempreal+data_wage_qtr(t)*data_wage_qtr(t-1)
!     tempint=tempint+1
!END DO
!ac_wage=(tempreal/REAL(tempint))-(wage_ave_q)*(wage_ave_q)
!ac_wage=ac_wage/var_wage


! ac_reall
tempreal=0.0_8
tempint=0
DO t=13, tmax_qtr-12
     tempreal=tempreal+data_lcocc_outflow5_qtr(t)*data_lcocc_outflow5_qtr(t-1)
     tempint=tempint+1
END DO
ac_reall5=(tempreal/REAL(tempint))-(reall_ave_q5)*(reall_ave_q5)
ac_reall5=ac_reall5/var_reall5


! NEW WAY OF CALCULATING
!CALL UNBIASED_CORRELATION( tmax_qtr-5, data_lcocc_outflow5_qtr(3:tmax_qtr-2), data_lcocc_outflow5_qtr(3:tmax_qtr-2), 999.99_8, 1, ac_reall5, tempreal, tempint)
!CALL UNBIASED_CORRELATION( tmax_qtr-5, data_lu5_qtr(3:tmax_qtr-2), data_lv5_qtr(3:tmax_qtr-2), 999.99_8, 0, corr_uv5_2, tempreal, tempint)







!====================================================
!========== LIFECYCLE CORRELATIONS AND VARIANCES
!=====================================================


!(data_lu5_qtr(13:tmax_qtr-12), tmax_qtr-24)

var_uyoung=VARCALCUL(data_u_young_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_uprime=VARCALCUL(data_u_prime_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_jfyoung=VARCALCUL(data_ljf5_young_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_jfprime=VARCALCUL(data_ljf5_prime_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_jfocc=VARCALCUL(data_lpocc5_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_jfnocc=VARCALCUL(data_lpnocc5_qtr(13:tmax_qtr-12) , tmax_qtr-24)

var_sepyoung=VARCALCUL(data_sep_y_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_lsepyoung=VARCALCUL(data_lsep_y_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_sepprime=VARCALCUL(data_sep_p_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_lsepprime=VARCALCUL(data_lsep_p_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_cm=VARCALCUL(data_cocc_outflow_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_cmyoung=VARCALCUL(data_cocc_young_outflow_qtr(13:tmax_qtr-12) , tmax_qtr-24)
var_cmprime=VARCALCUL(data_cocc_prime_outflow_qtr(13:tmax_qtr-12) , tmax_qtr-24)

relstdev_occmob_y=sqrt(var_cm)/sqrt(var_prod)

corr_uyoungprod=CORRCALCUL(data_u_young_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_uprimeprod=CORRCALCUL(data_u_prime_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_jfyoungprod=CORRCALCUL(data_ljf_young_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_jfprimeprod=CORRCALCUL(data_ljf_prime_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_sepyoungprod=CORRCALCUL(data_lsep_y_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_sepprimeprod=CORRCALCUL(data_lsep_p_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_cmyoungprod=CORRCALCUL(data_cocc_young_outflow_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_cmprimeprod=CORRCALCUL(data_cocc_prime_outflow_qtr(13:tmax_qtr-12) , data_prod_qtr(13:tmax_qtr-12) , tmax_qtr-24)

corr_jfyoungunempl=CORRCALCUL(data_ljf_young_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_jfprimeunempl=CORRCALCUL(data_ljf_prime_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_sepyoungunempl=CORRCALCUL(data_lsep_y_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_sepprimeunempl=CORRCALCUL(data_lsep_p_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_cmyoungunempl=CORRCALCUL(data_cocc_young_outflow_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)
corr_cmprimeunempl=CORRCALCUL(data_cocc_prime_outflow_qtr(13:tmax_qtr-12) , data_u_qtr(13:tmax_qtr-12) , tmax_qtr-24)






!$OMP CRITICAL
!CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
!WRITE(10,*) 'time=', time_string(2), 'before regressions, on thread:', threadnumber


ALLOCATE(Xjf(tmax_qtr-2,2), Yjf(1:tmax_qtr-2,1))


!Xjf(:,1)=1
!Xjf(:,2)=data_theta_qtr(1: tmax_qtr-2)
!Yjf(1:tmax_qtr-2,1)=data_ljf_qtr(1:tmax_qtr-2)
!
!CALL multivar_regression(Yjf, Xjf, tmax_qtr-2, 2, betajf,errmsg)
!
!IF (verbose_results_l1==1) THEN
!WRITE(10,*) ' '
!WRITE(10,*) '======> elasticity of jf wrt theta:', betajf(2)
!WRITE(10,*) ' '
!END

! CALCULATE ELASTICITIES OF INTEREST!!!


! 1. EMPIRICAL ELASTICITY OF THE MATCHING FUNCTION
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_theta_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ljf_qtr(13:tmax_qtr-12)

CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
IF (verbose_results_l1==1) WRITE(10,*) 'elasticity of the matching function:', betajf(2)
!WRITE(*,*) 'elasticity of the matching function:', betajf(2)
elmatching_ave=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'elmatching_ave'
END IF


! 2. UNEMP with productivity
Xjf(1:tmax_qtr-24,2)=data_prod_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_u_qtr(13:tmax_qtr-12)

CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
unemp_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'unemp_prod_elasticity'
END IF


! 3. UNEMPLOYMENT OF YOUNG WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_u_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
u_young_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u_young_prod_elasticity'
END IF


! 4. UNEMPLOYMENT OF PRIME-AGED WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_u_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
u_prime_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'u_prime_prod_elasticity'
END IF


! 5a. SEPARATION OF YOUNG WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lsep_y_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_young_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_young_prod_elasticity'
END IF


! 5b. SEPARATION OF PRIME-AGED WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lsep_p_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prime_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prime_prod_elasticity'
END IF


! 5a. SEPARATION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lsep_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prod_elasticity'
END IF



! 6a1. COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_inflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_inflow_prod_elasticity'
END IF


! 6a2. YOUNG COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_young_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_young_inflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_young_inflow_prod_elasticity'
END IF


! 6a3. PRIME COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_prime_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_prime_inflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_prime_inflow_prod_elasticity'
END IF



! 6a4. COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_outflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_outflow_prod_elasticity'
END IF


! 6a5. YOUNG COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_young_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_young_outflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_young_outflow_prod_elasticity'
END IF


! 6a6. PRIME COMPOSITION WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_cocc_prime_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_prime_outflow_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_prime_outflow_prod_elasticity'
END IF



! 6b. JFOCC WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lpocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc_prod_elasticity'
END IF


! 6c. JFNOCC WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lpnocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc_prod_elasticity'
END IF



! 7a. JF OF YOUNG WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_young_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_young_prod_elasticity'
END IF



! 7b. JF OF PRIME-AGED WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_prime_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_prime_prod_elasticity'
END IF


! 7c. JF WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_prod_elasticity'
END IF


!! FIVE QUARTER SMOOTHED VARS

! 6b2. JFOCC WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lpocc5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc5_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc5_prod_elasticity'
END IF


! 6c2. JFNOCC WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_lpnocc5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc5_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc5_prod_elasticity'
END IF



! 7a2. JF OF YOUNG WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf5_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_young_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_young_prod_elasticity'
END IF


! 7b2. JF OF PRIME-AGED WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf5_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_prime_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_prime_prod_elasticity'
END IF


! 7c2. JF WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ljf5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_prod_elasticity'
END IF




! X. WAGE WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_wage_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
wage_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'wage_prod_elasticity'
END IF



! XX. PROPORTIONS U WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_l_ult5wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5w_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5w_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_u5lt15wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop15w_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop15w_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_u15lt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop27w_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop27w_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ugt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropgt27w_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropgt27w_prod_elasticity'
END IF



! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_l_ult3m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult5m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult9m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_prod_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_prod_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_prod_elasticity'
END IF







! XX. SEMI-semielasticity PROPORTIONS U WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ult5wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5w_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5w_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_u5lt15wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop15w_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop15w_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_u15lt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop27w_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop27w_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ugt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropgt27w_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropgt27w_prod_semielasticity'
END IF



! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_prod_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_prod_semielasticity'
END IF

Yjf(1:tmax_qtr-24,1)=data_ugt13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropg13m_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropg13m_prod_semielasticity'
END IF


! XX. PROPORTIONS U WITH prod_yngUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_prod_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_prod_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_prod_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_prod_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_prod_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_prod_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_prod_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_prod_yng_semielasticity'
    END IF


! XX. PROPORTIONS U WITH prod_prmUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_prod_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_prod_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_prod_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_prod_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_prod_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_prod_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_prod_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_prod_prm_semielasticity'
END IF




!!-----------------------
!! ELASTICITIES WITH UNEMPLOYMENT

Xjf(1:tmax_qtr-24,2)=data_u_qtr(13: tmax_qtr-12)

! 8a. SEP YOUNG WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_lsep_y_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_young_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_young_unemp_elasticity'
END IF


! 8b. SEP PRIME WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_lsep_p_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prime_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prime_unemp_elasticity'
END IF



! 8b. SEP WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_lsep_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_unemp_elasticity'
END IF



! 9a1. COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_inflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_inflow_unemp_elasticity'
END IF


! 9a2. YOUNG COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_young_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_young_inflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_young_inflow_unemp_elasticity'
END IF


! 9a3. PRIME COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_prime_inflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_prime_inflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_prime_inflow_unemp_elasticity'
END IF


! 9a4. OUTFLOW COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_outflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_outflow_unemp_elasticity'
END IF



! 9a5. YOUNG OUTFLOW COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_young_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_young_outflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_young_outflow_unemp_elasticity'
END IF



! 9a6. PRIME OUTFLOW COMPOSITION WITH UNEMPLOYMENT

Yjf(1:tmax_qtr-24,1)=data_cocc_prime_outflow_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
cocc_prime_outflow_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'cocc_prime_outflow_unemp_elasticity'
END IF



! 9b. JFOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_lpocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc_unemp_elasticity'
END IF


! 9c. JFNOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_lpnocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc_unemp_elasticity'
END IF




! 10a. JF OF YOUNG WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_ljf_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_young_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_young_unemp_elasticity'
END IF


! 10b. JF OF PRIME-AGED WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_ljf_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_prime_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_prime_unemp_elasticity'
END IF


! 10c. JF OF ALL  WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_ljf_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_unemp_elasticity'
END IF




! 12a. ELASTICITY U w/ v
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_v_qtr(13:tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_u_qtr(13: tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
el_u_with_v=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_u_with_v'
END IF


! 12b. ELASTICITY V w/ U
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_u_qtr(13:tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_v_qtr(13: tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
el_v_with_u=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_v_with_u'
END IF


! 12c. ELASTICITY U w/ v  -- SMOOTHED
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_lv5_qtr(13:tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_lu5_qtr(13: tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
el_u5_with_v5=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_u5_with_v5'
END IF


! 12d. ELASTICITY V w/ U  -- SMOOTHED
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_lu5_qtr(13:tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_lv5_qtr(13: tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
el_v5_with_u5=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_v5_with_u5'
END IF



! 13. ELASTICITY U w/ P
Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_prod_qtr(13:tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_u_qtr(13: tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
el_u_with_p=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'el_u_with_p'
END IF



!=============================================
!   SEMI ELASTICITIES
!==============================================

!==========================================
! with output
Xjf(1:tmax_qtr-24,2)=data_prod_qtr(13: tmax_qtr-12)

! 1a. JF OF YOUNG WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_young_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_young_prod_semielasticity'
END IF


! 1b. JF OF PRIME-AGED WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_prime_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_prime_prod_semielasticity'
END IF



! 1c. JF OF PRIME-AGED WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_prod_semielasticity=betajf(2)
jf_prod_semielasticity=-999.99
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_prod_semielasticity'
END IF


! 2b. JFOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_pocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc5_prod_semielasticity=betajf(2)
jfocc_prod_semielasticity=-999.99
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc5_prod_semielasticity'
END IF


! 2c. JFNOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_pnocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc5_prod_semielasticity=betajf(2)
jfnocc_prod_semielasticity=-999.99
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc5_prod_semielasticity'
END IF


! 3a. SEP YOUNG WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_sep_y_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_young_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_young_prod_semielasticity'
END IF


! 3b. SEP PRIME WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_sep_p_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prime_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prime_prod_semielasticity'
END IF



! 3c. SEP WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_sep_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prod_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prod_semielasticity'
END IF



!===================================
! with UNEMPLOYMENT

Xjf(1:tmax_qtr-24,2)=data_u_qtr(13: tmax_qtr-12)

! 1a. JF OF YOUNG WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_young_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_young_unemp_semielasticity'
END IF


! 1b. JF OF PRIME-AGED WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_prime_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_prime_unemp_semielasticity'
END IF


! 1c. JF OF PRIME-AGED WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_jf_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf_unemp_semielasticity'
END IF



! 2b. JFOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_pocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc_unemp_semielasticity'
END IF


! 2c. JFNOCC WITH UNEMPLOYMENT
Yjf(1:tmax_qtr-24,1)=data_pnocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc_unemp_semielasticity'
END IF



! 3a. SEP YOUNG WITH UNEMPL
Yjf(1:tmax_qtr-24,1)=data_sep_y_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_young_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_young_unemp_semielasticity'
END IF


! 3b. SEP PRIME WITH UNEMPL
Yjf(1:tmax_qtr-24,1)=data_sep_p_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_prime_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_prime_unemp_semielasticity'
END IF



! 3c. SEP WITH UNEMPL
Yjf(1:tmax_qtr-24,1)=data_sep_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sep_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sep_unemp_semielasticity'
END IF



!! UNEMPLOYMENT DURATION ELASTICITY AND SEMIELASTICITY


! XX. PROPORTIONS U WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_l_ult5wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5w_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_u5lt15wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop15w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop15w_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_u15lt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop27w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop27w_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ugt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropgt27w_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropgt27w_unemp_elasticity'
END IF



! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_l_ult3m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult5m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult9m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_elasticity'
    END IF

!! YOUNG

Yjf(1:tmax_qtr-24,1)=data_l_ult3m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult5m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult9m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_yng_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult13m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_yng_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_yng_elasticity'
    END IF

!! PRIME

    Yjf(1:tmax_qtr-24,1)=data_l_ult3m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult5m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult9m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_prm_elasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_l_ult13m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_prm_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_prm_elasticity'
END IF



!!!!! ----> SEMI ELASTICITY WITH UNEMPLOYMENT


! XX. PROPORTIONS U WITH PRODUCTIVITY
Yjf(1:tmax_qtr-24,1)=data_ult5wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_u5lt15wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop15w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop15w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_u15lt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop27w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop27w_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ugt27wk_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropgt27w_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropgt27w_unemp_semielasticity'
END IF



! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_semielasticity'
    END IF

Yjf(1:tmax_qtr-24,1)=data_ugt13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_upropg13m_unemp_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_upropg13m_unemp_semielasticity'
    END IF

!! SMOOTHED

    Yjf(1:tmax_qtr-24,1)=data_sm5_ult3m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sm_hp_uprop3m_u_semi_el=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sm_hp_uprop3m_u_semi_el'
END IF


Yjf(1:tmax_qtr-24,1)=data_sm5_ult5m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sm_hp_uprop5m_u_semi_el=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sm_hp_uprop5m_u_semi_el'
END IF


Yjf(1:tmax_qtr-24,1)=data_sm5_ult9m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sm_hp_uprop9m_u_semi_el=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sm_hp_uprop9m_u_semi_el'
END IF


Yjf(1:tmax_qtr-24,1)=data_sm5_ult13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sm_hp_uprop13m_u_semi_el=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sm_hp_uprop13m_u_semi_el'
    END IF

Yjf(1:tmax_qtr-24,1)=data_sm5_ugt13m_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
sm_hp_upropg13m_u_semi_el=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'sm_hp_upropg13m_u_semi_el'
    END IF




!! YOUNG

    ! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_yng_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_yng_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_yng_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_yng_semielasticity'
    END IF

    !! PRIME
    ! XX. PROPORTIONS U WITH PRODUCTIVITY FROM ****** SIPP *******
Yjf(1:tmax_qtr-24,1)=data_ult3m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop3m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop3m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult5m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop5m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop5m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult9m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop9m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop9m_unemp_prm_semielasticity'
END IF


Yjf(1:tmax_qtr-24,1)=data_ult13m_prm_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
hp_uprop13m_unemp_prm_semielasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'hp_uprop13m_unemp_prm_semielasticity'
END IF


!! FIVE QUARTER SMOOTHED VARS

! 6b2. JFOCC WITH UNEMP
Yjf(1:tmax_qtr-24,1)=data_lpocc5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfocc5_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfocc5_unemp_elasticity'
END IF


! 6c2. JFNOCC WITH UNEMP
Yjf(1:tmax_qtr-24,1)=data_lpnocc5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jfnocc5_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jfnocc5_unemp_elasticity'
END IF



! 7a2. JF OF YOUNG WITH UNEMP
Yjf(1:tmax_qtr-24,1)=data_ljf5_young_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_young_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_young_unemp_elasticity'
END IF


! 7b2. JF OF PRIME-AGED WITH UNEMP
Yjf(1:tmax_qtr-24,1)=data_ljf5_prime_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_prime_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_prime_unemp_elasticity'
END IF


! 7c2. JF WITH UNEMP
Yjf(1:tmax_qtr-24,1)=data_ljf5_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
jf5_unemp_elasticity=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'jf5_unemp_elasticity'
END IF



!! XXX DURATION BEHAVIOR: ABS CHANGES

Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_occ_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_occ_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_with_hpu'
END IF


Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_young_occ_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_occ_young_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_young_with_hpu'
END IF


Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_prime_occ_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_occ_prime_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_occ_prime_with_hpu'
END IF



Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_nocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_nocc_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_with_hpu'
END IF


Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_young_nocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_nocc_young_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_young_with_hpu'
END IF


Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_prime_nocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_nocc_prime_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_prime_with_hpu'
END IF

Xjf(:,1)=1
Xjf(1:tmax_qtr-24,2)=data_ulev_qtr(13: tmax_qtr-12)
Yjf(1:tmax_qtr-24,1)=data_ucompldur_prime_nocc_qtr(13:tmax_qtr-12)
CALL multivar_regression(Yjf(1:tmax_qtr-24,:), Xjf(1:tmax_qtr-24,:), tmax_qtr-24, 2, betajf,errmsg)
udur_nocc_prime_with_hpu=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'udur_nocc_prime_with_hpu'
END IF



!!! MUELLER MOMENTS

Xjf(:,1)=1

DO t=1,tmax_qtr/4-8
    Xjf(t,2)=data_u_yr(t+4)
    Yjf(t,1)=data_sep_y_yr(t+4)-data_sep_p_yr(t+4)
END DO
CALL multivar_regression(Yjf(1:(tmax_qtr/4-8),:), Xjf(1:(tmax_qtr/4-8),:), (tmax_qtr/4-8), 2, betajf,errmsg)
cycl_sep_age_shift_muellermom=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'MUELLER MOMENT REGRESSION SEP'
END IF

 !       data_jf_y_yr
    !       data_jf_p_yr
    !       data_u_y_yr
    !       data_u_p_yr

! job finding
DO t=1,tmax_qtr/4-8
    Xjf(t,2)=data_u_yr(t+4)
    Yjf(t,1)=data_jf_y_yr(t+4)-data_jf_p_yr(t+4)
END DO
CALL multivar_regression(Yjf(1:(tmax_qtr/4-8),:), Xjf(1:(tmax_qtr/4-8),:), (tmax_qtr/4-8), 2, betajf,errmsg)
cycl_jf_age_shift_muellermom=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'MUELLER MOMENT REGRESSION JF'
    END IF


! unemployment
DO t=1,tmax_qtr/4-8
    Xjf(t,2)=data_u_yr(t+4)
    Yjf(t,1)=data_u_y_yr(t+4)-data_u_p_yr(t+4)
END DO
CALL multivar_regression(Yjf(1:(tmax_qtr/4-8),:), Xjf(1:(tmax_qtr/4-8),:), (tmax_qtr/4-8), 2, betajf,errmsg)
cycl_u_age_shift_muellermom=betajf(2)
IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
    WRITE(10, '(14(F10.4))') thetal
    WRITE(10,*) 'MUELLER MOMENT REGRESSION U'
END IF


!!==========================================
!!  DURATION REGRESSIONS Occ/Nocc with the cycle
!!==========================================

counter1=tmax_qtr-2
ALLOCATE(hp_scratch(4*(counter1),3), data_temp_qtr(counter1), data_temp_qtr2(counter1))


udur_mvec_occ_with_u=0.0_8
udur_mvec_occ_young_with_u=0.0_8
udur_mvec_occ_prime_with_u=0.0_8
udur_mvec_nocc_with_u=0.0_8
udur_mvec_nocc_young_with_u=0.0_8
udur_mvec_nocc_prime_with_u=0.0_8

udur_mvec_occ_with_hpu=0.0_8
udur_mvec_occ_young_with_hpu=0.0_8
udur_mvec_occ_prime_with_hpu=0.0_8
udur_mvec_nocc_with_hpu=0.0_8
udur_mvec_nocc_young_with_hpu=0.0_8
udur_mvec_nocc_prime_with_hpu=0.0_8


!udur_mvec_occ_with_lu=0.0_8
!udur_mvec_occ_young_with_lu=0.0_8
!udur_mvec_occ_prime_with_lu=0.0_8
!udur_mvec_nocc_with_lu=0.0_8
!udur_mvec_nocc_young_with_lu=0.0_8
!udur_mvec_nocc_prime_with_lu=0.0_8
!
!udur_mvec_occ_with_hplu=0.0_8
!udur_mvec_occ_young_with_hplu=0.0_8
!udur_mvec_occ_prime_with_hplu=0.0_8
!udur_mvec_nocc_with_hplu=0.0_8
!udur_mvec_nocc_young_with_hplu=0.0_8
!udur_mvec_nocc_prime_with_hplu=0.0_8
!


!!==> ADD TIME DETRENDING IN 30 YEAR WINDOWS, BELOW  



!WRITE(*,*) 'entering  udur_mvec regressions'

! ---- occ changers - ALL ----


DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_cocc_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_cocc_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt

    ! quarterly ts average
    ! udur_mvec_occ_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! detrend Yjf, detrend data_uraw_qtr in 30 year windows if switch is on. 
    
    
    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_with_u(xcnt)'
    END IF



    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_with_hpu(xcnt)'
    END IF




END DO !xcnt

! ---- occ changers - YOUNG ----
DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_cocc_young_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_cocc_young_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt

    ! quarterly ts average
    !udur_mvec_occ_young_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_young_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_young_with_u(xcnt)'
    END IF


    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_young_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_young_with_hpu(xcnt)'
    END IF


END DO !xcnt


! ---- occ changers - PRIME ----
DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_cocc_prime_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_cocc_prime_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt


    ! quarterly ts average
    !udur_mvec_occ_prime_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_prime_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_prime_with_u(xcnt)'
    END IF


    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_occ_prime_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_occ_prime_with_hpu(xcnt)'
    END IF


END DO !xcnt

! ---- nocc stayers - ALL ----
DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_nocc_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_nocc_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt


    ! quarterly ts average
    !udur_mvec_nocc_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_with_u(xcnt)'
    END IF


    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_with_hpu(xcnt)'
    END IF


END DO !xcnt

! ---- nocc stayers - YOUNG ----
DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_nocc_young_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_nocc_young_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt



    ! quarterly ts average
    ! udur_mvec_nocc_young_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_young_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_young_with_u(xcnt)'
    END IF


    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_young_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_young_with_hpu(xcnt)'
    END IF


END DO !xcnt


! ---- nocc stayers - PRIME ----
DO xcnt=1,6
    DO tcnt=1, tmax_qtr-2

        tempreal=0.0_8
        DO zcnt=xcnt, 18
        tempreal=tempreal+data_compduration_nocc_prime_qtr(tcnt,zcnt)*REAL(zcnt)
        END DO !zcnt

        IF(tempreal>0.0_8) THEN
            tempreal=tempreal/sum(data_compduration_nocc_prime_qtr(tcnt,xcnt:18))
        ELSE IF (tempreal==0.0_8) THEN
            tempreal=0.0_8
        END IF

        Yjf(tcnt,1)=tempreal

    END DO !tcnt


    ! quarterly ts average
    ! udur_mvec_nocc_prime_ave(xcnt)=sum(Yjf(1:tmax_qtr-2,1))/REAL(tmax_qtr-2)

    ! run overall regression
    Xjf(:,1)=1
    Xjf(:,2)=data_uraw_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_prime_with_u(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_prime_with_u'
    END IF


    ! run HP filtered regression on hpu levels
    counter1=tmax_qtr-2
    data_temp_qtr2(1:counter1)=Yjf(1:counter1,1)
    CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,counter1,hpfilterfactor,0)
    Yjf(1:counter1,1)=data_temp_qtr(1:counter1)
    Xjf(:,1)=1
    Xjf(:,2)=data_ulev_qtr(1: tmax_qtr-2)
    CALL multivar_regression(Yjf(13:tmax_qtr-12,:), Xjf(13:tmax_qtr-12,:), tmax_qtr-24, 2, betajf,errmsg)
    udur_mvec_nocc_prime_with_hpu(xcnt)=betajf(2)
    IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
        WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
        WRITE(10, '(14(F10.4))') thetal
        WRITE(10,*) 'udur_mvec_nocc_prime_with_hpu'
    END IF


    END DO !xcnt

!moverstayer_regr_lindetrending_if
moverstayer_regr_lindetrending_if: &
IF(moverstayer_regr_lindetrending==1 ) THEN 
        !--- ONLY ALL WORKERS' RESPONSE IS LINEARLY DETRENDED, FOR OCC AND NOCC, BUT FOR 4 DIFFERENT MEASURES OF UNEMPLOYMENT (2x UNEMP, 2x HP FILTERED)
        !--- ALSO GET RID OF THE CONSTANT, JUST TO DEAL WITH THE RESIDUALS

        !WRITE(*,*) 'entering  moverstayer_regr_lindetrending'

        tmax_swindow_qtr=tmax_swindow_wks_raw/12
        !data_ucompldur_nocc_qtr, data_ucompldur_occ_qtr
        tempcounter=((tmax_qtr-2)/tmax_swindow_qtr)
        tempcounter2=tempcounter*tmax_swindow_qtr       ! maximum time for all full superwindows
            
                        ! linearly detrend data_ucompldur_nocc_qtr
                        
                        Xjf(:,1)=1
                        
                        ! data_ucompldur_nocc_qtr is already HP FILTERED, so have to reconstruct it
                        
                        DO tcnt=1, tempcounter2

                            tempreal=0.0_8
                            DO zcnt=1, 18
                            tempreal=tempreal+data_compduration_nocc_qtr(tcnt,zcnt)*REAL(zcnt)
                            END DO !zcnt

                            IF(tempreal>0.0_8) THEN
                                tempreal=tempreal/sum(data_compduration_nocc_qtr(tcnt,1:18))
                            ELSE IF (tempreal==0.0_8) THEN
                                tempreal=0.0_8
                            END IF

                            Yjf(tcnt,1)=tempreal

                        END DO 
                        
                        
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1          ! tempcounter1 is maximum time within superwindow
                            DO tcnt=1, tmax_swindow_qtr
                                Xjf(counter1+tcnt-1,2)=REAL(tcnt)
                            END DO 
                            ! linear detrending within superwindow: estimate
                            CALL multivar_regression(Yjf(counter1:tempcounter1,:), Xjf(counter1:tempcounter1,:), tmax_swindow_qtr, 2, betajf,errmsg)
                                IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                                    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                                    WRITE(10,*) 'linear detrending data_ucompldur_nocc_qtr'
                                END IF

                                ! linear detrending within superwindow: residuals of estimation
                                DO tcnt=1, tmax_swindow_qtr
                                data_ucompldur_nocc_qtr(counter1+tcnt-1)=Yjf(counter1+tcnt-1,1)-betajf(1)-betajf(2)*Xjf(counter1+tcnt-1,2)
                                END DO 
                        END DO 
                        

            
                        ! linearly detrend data_ucompldur_occ_qtr
                        Xjf(:,1)=1
                        
                        ! data_ucompldur_occ_qtr is already HP FILTERED, so have to reconstruct it
                        
                        DO tcnt=1, tempcounter2

                            tempreal=0.0_8
                            DO zcnt=1, 18
                            tempreal=tempreal+data_compduration_cocc_qtr(tcnt,zcnt)*REAL(zcnt)
                            END DO !zcnt

                            IF(tempreal>0.0_8) THEN
                                tempreal=tempreal/sum(data_compduration_cocc_qtr(tcnt,1:18))
                            ELSE IF (tempreal==0.0_8) THEN
                                tempreal=0.0_8
                            END IF

                            Yjf(tcnt,1)=tempreal

                        END DO 
                        
                        
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1          ! tempcounter1 is maximum time within superwindow
                            
                            DO tcnt=1, tmax_swindow_qtr
                                Xjf(counter1+tcnt-1,2)=REAL(tcnt)
                            END DO 
                            ! linear detrending within superwindow: estimate
                            CALL multivar_regression(Yjf(counter1:tempcounter1,:), Xjf(counter1:tempcounter1,:), tmax_swindow_qtr, 2, betajf,errmsg)
                                IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                                    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                                    WRITE(10,*) 'linear detrending data_ucompldur_occ_qtr'
                                END IF

                                ! linear detrending within superwindow: residuals of estimation
                                DO tcnt=1, tmax_swindow_qtr
                                    data_ucompldur_occ_qtr(counter1+tcnt-1)=Yjf(counter1+tcnt-1,1)-betajf(1)-betajf(2)*Xjf(counter1+tcnt-1,2)
                                END DO 
                        END DO 
                        

            
                        !!=== LINEARLY DETRENDED U LEVEL SERIES 
            
                        ! linearly detrend data_u  non-logged: data_temp_qtr
                        Yjf(1:tempcounter2,1)=data_uraw_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1          ! tempcounter1 is maximum time within superwindow
                            DO tcnt=1, tmax_swindow_qtr
                                Xjf(counter1+tcnt-1,2)=REAL(tcnt)
                            END DO 
                            ! linear detrending within superwindow: estimate
                            CALL multivar_regression(Yjf(counter1:tempcounter1,:), Xjf(counter1:tempcounter1,:), tmax_swindow_qtr, 2, betajf,errmsg)
                                IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                                    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                                    WRITE(10,*) 'linear detrending data_ucompldur_nocc_qtr'
                                END IF

                                ! linear detrending within superwindow: residuals of estimation
                                DO tcnt=counter1, tempcounter1
                                Xjf(tcnt,2)=data_uraw_qtr(tcnt)-betajf(1)-betajf(2)*Xjf(tcnt,2)
                                END DO 
                        END DO 
                        
                        !! RUN REGRESSIONS
                        !! 1) NOCC DUR ON LIN DETRENDED U (LEVEL)
                        !! 2) OCC DUR ON LIN DETRENDED U (LEVEL)   
    
                        !! 1) NOCC DUR ON LIN DETRENDED U (LEVEL)
                        Yjf(1:tempcounter2,1)=data_ucompldur_nocc_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LINEARLY DETRENDED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_nocc_with_u=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_nocc_with_u LIN DETRENDED'
                        END IF

    
                        ! linearly detrend data_u  logged: data_temp_qtr2
                        Yjf(1:tempcounter2,1)=data_ucompldur_occ_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LINEARLY DETRENDED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_occ_with_u=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_occ_with_u LIN DETRENDED'
                        END IF

            !!=== LINEARLY DETRENDED U LOG SERIES 
            
                        ! linearly detrend data_u  non-logged: data_temp_qtr
                        Yjf(1:tempcounter2,1)=log(data_uraw_qtr(1:tempcounter2))
                        Xjf(:,1)=1
                        
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1         ! tempcounter1 is maximum time within superwindow
                            DO tcnt=1, tmax_swindow_qtr
                                Xjf(counter1+tcnt-1,2)=tcnt
                            END DO 
                            ! linear detrending within superwindow: estimate
                            CALL multivar_regression(Yjf(counter1:tempcounter1,:), Xjf(counter1:tempcounter1,:), tmax_swindow_qtr, 2, betajf,errmsg)
                                IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                                    WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                                    WRITE(10,*) 'linear detrending data_ucompldur_nocc_qtr'
                                END IF

                                ! linear detrending within superwindow: residuals of estimation
                                DO tcnt=counter1, tempcounter1
                                Xjf(tcnt,2)=log(data_uraw_qtr(tcnt))-betajf(1)-betajf(2)*Xjf(tcnt,2)
                                END DO 
                        END DO 
                        
                        !! RUN REGRESSIONS
                        !! 3) NOCC DUR ON LIN DETRENDED U (LOG)
                        !! 4) OCC DUR ON LIN DETRENDED U (LOG)   
    
                        !! 3) NOCC DUR ON LIN DETRENDED U (LOG)
                        Yjf(1:tempcounter2,1)=data_ucompldur_nocc_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LINEARLY DETRENDED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_nocc_with_lu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_nocc_with_lu LIN DETRENDED'
                        END IF

                        !! 4) OCC DUR ON LIN DETRENDED U (LOG)    
                        ! linearly detrend data_u  logged: data_temp_qtr2
                        Yjf(1:tempcounter2,1)=data_ucompldur_occ_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LOG LINEARLY DETRENDED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_occ_with_lu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_occ_with_lu LIN DETRENDED'
                        END IF

            
        
                        !!=== HP FILTERED U LEVEL SERIES WITHIN SUPERWINDOW
            
            
                        
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1          ! tempcounter1 is maximum time within superwindow
                            
                            !! filter within superwindow from counter1 to tempcounter1, of length tmax_swindow_qtr
                            data_temp_qtr2(1:tmax_swindow_qtr)=data_uraw_qtr(counter1:tempcounter1)
                            CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,tmax_swindow_qtr,hpfilterfactor,0)
                            Xjf(counter1:tempcounter1,2)=data_temp_qtr(1:tmax_swindow_qtr)
                        
                        END DO 
                        
    
                        !! REGRESSIONS
                        !! 5) NOCC DURATION WITH HP U
                        !! 6) OCC UDURATION WITH HP U 
                        
                        !! 5) NOCC DUR ON HP FILTERED U (LEVEL)
                        Yjf(1:tempcounter2,1)=data_ucompldur_nocc_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR HP FILTERED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_nocc_with_hpu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_nocc_with_hpu HPF + LIN DETRENDED'
                        END IF

                        !! 6) OCC DUR ON HP FILTERED U (LEVEL)    
                        ! linearly detrend data_u  logged: data_temp_qtr2
                        Yjf(1:tempcounter2,1)=data_ucompldur_occ_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR HP FILTERED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_occ_with_hpu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_occ_with_hpu HPF+ LIN DETRENDED'
                        END IF 
    
    
                        !! LOG UNEMPLOYMENT RATE -  HP FILTERING 
                        DO counter1=1, tempcounter2, tmax_swindow_qtr       ! cycle through superwindows
                            tempcounter1=counter1+tmax_swindow_qtr-1          ! tempcounter1 is maximum time within superwindow
                            
                            !! filter within superwindow from counter1 to tempcounter1, of length tmax_swindow_qtr
                            data_temp_qtr2(1:tmax_swindow_qtr)=log(data_uraw_qtr(counter1:tempcounter1))
                            CALL HPFILT(data_temp_qtr2,data_temp_qtr,hp_scratch,tmax_swindow_qtr,hpfilterfactor,0)
                            Xjf(counter1:tempcounter1,2)=data_temp_qtr(1:tmax_swindow_qtr)
                        
                        END DO 
                        
                                            !! REGRESSIONS
                        !! 7) NOCC DURATION WITH HP LOG U
                        !! 8) OCC UDURATION WITH HP LOG U 
                        
                        !! 7) NOCC DUR ON HP FILTERED U (LOG )
                        Yjf(1:tempcounter2,1)=data_ucompldur_nocc_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LOG HP FILTERED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_nocc_with_hplu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_nocc_with_hplu HPF + LIN DETRENDED'
                        END IF

                        !! 8) OCC DUR ON HP FILTERED U (LOG )    
                        ! linearly detrend data_u  logged: data_temp_qtr2
                        Yjf(1:tempcounter2,1)=data_ucompldur_occ_qtr(1:tempcounter2)
                        Xjf(:,1)=1
                        !Xjf(:,2) ALREADY SET TO DATA_URAW_QTR LOG HP FILTERED ABOVE
                        CALL multivar_regression(Yjf(1:tempcounter2,:), Xjf(1:tempcounter2,:), tempcounter2, 2, betajf,errmsg)
                        udur_occ_with_hplu=betajf(2)
                        IF(errmsg>0 .AND. print_regress_errmsg_ind==1) THEN
                            WRITE(10,*) 'error in regression, code:', errmsg, 'data/time', time_string(1), time_string(2)
                            WRITE(10, '(14(F10.4))') thetal
                            WRITE(10,*) 'udur_occ_with_hplu HPF+ LIN DETRENDED'
                        END IF 
    
    
        
END IF moverstayer_regr_lindetrending_if


DEALLOCATE(hp_scratch, data_temp_qtr, data_temp_qtr2)






hp_var_uprop_3m_ave=VARCALCUL(data_ult3m_qtr, tmax_qtr)
hp_var_uprop_5m_ave=VARCALCUL(data_ult5m_qtr, tmax_qtr)
hp_var_uprop_9m_ave=VARCALCUL(data_ult9m_qtr, tmax_qtr)
hp_var_uprop_13m_ave=VARCALCUL(data_ult13m_qtr, tmax_qtr)
hp_var_l_uprop_3m_ave=VARCALCUL(data_l_ult3m_qtr, tmax_qtr)
hp_var_l_uprop_5m_ave=VARCALCUL(data_l_ult5m_qtr, tmax_qtr)
hp_var_l_uprop_9m_ave=VARCALCUL(data_l_ult9m_qtr, tmax_qtr)
hp_var_l_uprop_13m_ave=VARCALCUL(data_l_ult13m_qtr, tmax_qtr)

hp_var_uprop_yng_3m_ave=VARCALCUL(data_ult3m_yng_qtr, tmax_qtr)
hp_var_uprop_yng_5m_ave=VARCALCUL(data_ult5m_yng_qtr, tmax_qtr)
hp_var_uprop_yng_9m_ave=VARCALCUL(data_ult9m_yng_qtr, tmax_qtr)
hp_var_uprop_yng_13m_ave=VARCALCUL(data_ult13m_yng_qtr, tmax_qtr)
hp_var_l_uprop_yng_3m_ave=VARCALCUL(data_l_ult3m_yng_qtr, tmax_qtr)
hp_var_l_uprop_yng_5m_ave=VARCALCUL(data_l_ult5m_yng_qtr, tmax_qtr)
hp_var_l_uprop_yng_9m_ave=VARCALCUL(data_l_ult9m_yng_qtr, tmax_qtr)
hp_var_l_uprop_yng_13m_ave=VARCALCUL(data_l_ult13m_yng_qtr, tmax_qtr)


hp_var_uprop_prm_3m_ave=VARCALCUL(data_ult3m_prm_qtr, tmax_qtr)
hp_var_uprop_prm_5m_ave=VARCALCUL(data_ult5m_prm_qtr, tmax_qtr)
hp_var_uprop_prm_9m_ave=VARCALCUL(data_ult9m_prm_qtr, tmax_qtr)
hp_var_uprop_prm_13m_ave=VARCALCUL(data_ult13m_prm_qtr, tmax_qtr)
hp_var_l_uprop_prm_3m_ave=VARCALCUL(data_l_ult3m_prm_qtr, tmax_qtr)
hp_var_l_uprop_prm_5m_ave=VARCALCUL(data_l_ult5m_prm_qtr, tmax_qtr)
hp_var_l_uprop_prm_9m_ave=VARCALCUL(data_l_ult9m_prm_qtr, tmax_qtr)
hp_var_l_uprop_prm_13m_ave=VARCALCUL(data_l_ult13m_prm_qtr, tmax_qtr)












DEALLOCATE(Xjf, Yjf)
!$OMP END CRITICAL



corr_uwage=huge(1.0_8)
corr_vwage=huge(1.0_8)
corr_thetawage=huge(1.0_8)
corr_sepwage=huge(1.0_8)
corr_jfwage=huge(1.0_8)
corr_wageprod=huge(1.0_8)
corr_wagereall=huge(1.0_8)
ac_wage=huge(1.0_8)
var_wage=huge(1.0_8)

    !******************************************************************
    !
    !VI. Moment distance (for SMM)
    !
    !******************************************************************
!
    ! TARGETING average transition rates, average old/young wage ratio,
    ! and moments of the average (unconditional) tenure distribution
      !$OMP CRITICAL

                    udur_occ_moment1=tot_udurationmatrix_m(8)/tot_udurationmatrix_m(4)
                    udur_occ_moment2=tot_udurationmatrix_m(12)/tot_udurationmatrix_m(8)


                     cmy_cmp_mom=tot_durationmatrix_cocc_young(1)/tot_durationmatrix_cocc_prime(1)
                    cmy_cmp_corr_mom=tot_durationmatrix_cr_cocc_young(1)/tot_durationmatrix_cr_cocc_prime(1)
                    cmy_cmp_mom2m=tot_durationmatrix_cocc_young(2)/tot_durationmatrix_cocc_prime(2)
                    cmy_cmp_corr_mom2m=tot_durationmatrix_cr_cocc_young(2)/tot_durationmatrix_cr_cocc_prime(2)
                    cmy_cmp_mom3m=tot_durationmatrix_cocc_young(3)/tot_durationmatrix_cocc_prime(3)
                    cmy_cmp_corr_mom3m=tot_durationmatrix_cr_cocc_young(3)/tot_durationmatrix_cr_cocc_prime(3)
                    cmy_cmp_mom4m=tot_durationmatrix_cocc_young(4)/tot_durationmatrix_cocc_prime(4)
                    cmy_cmp_corr_mom4m=tot_durationmatrix_cr_cocc_young(4)/tot_durationmatrix_cr_cocc_prime(4)
                    cmy_cmp_mom_ave=SUM(tot_durationmatrix_cocc_young(1:12))/SUM(tot_durationmatrix_cocc_prime(1:12))
                    cmy_cmp_corr_mom_ave=SUM(tot_durationmatrix_cr_cocc_young(1:12))/SUM(tot_durationmatrix_cr_cocc_prime(1:12))


                    occmob_3m_p33=tot_durationmatrix_cr_cocc_p33(3)
                    occmob_3m_p67=tot_durationmatrix_cr_cocc_p67(3)
                    occmob_6m_p33=tot_durationmatrix_cr_cocc_p33(6)
                    occmob_6m12_p67=(tot_durationmatrix_cr_cocc_p67(6)+tot_durationmatrix_cr_cocc_p67(7)+tot_durationmatrix_cr_cocc_p67(8)+ &
                                            tot_durationmatrix_cr_cocc_p67(9)+tot_durationmatrix_cr_cocc_p67(10)+tot_durationmatrix_cr_cocc_p67(11)+&
                                            tot_durationmatrix_cr_cocc_p67(12))/7.0_8



    ! DISTRIBUTION OF UNEMPLOYMENT RATES IN THE TIME SERIES
    unemp_ee_p5=wquantile(0.05_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p10=wquantile(0.10_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p25=wquantile(0.25_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p50=wquantile(0.50_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p75=wquantile(0.75_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p90=wquantile(0.90_8,up_distribution_sim(:),p_distribution_sim(:))
    unemp_ee_p95=wquantile(0.95_8,up_distribution_sim(:),p_distribution_sim(:))






                    !=====================
                    !  EMPIRICAL DATA MOMENTS
                    !====================

                    mom_data=0.0_8          ! set all moments to zero, as default

                    mom_data(1)=1.0_8                  ! average productivity
                    mom_data(2)=ac_p_data
                    mom_data(3)=sd_p_data
                    mom_data(4)=elmatch_data
                    


                    !mom_data(5)=tot_durationmatrix_cocc_data(1)
                    !mom_data(7)=tot_durationmatrix_cocc_data(2)
                    !mom_data(9)=tot_durationmatrix_cocc_data(4)
                    !mom_data(11)=tot_durationmatrix_cocc_data(8)
                    !mom_data(13)=tot_durationmatrix_cocc_data(10)
                    !mom_data(15)=tot_durationmatrix_cocc_data(12)


                    mom_data(6)=tot_durationmatrix_data(2)
                    mom_data(8)=tot_durationmatrix_data(4)
                    mom_data(10)=tot_durationmatrix_data(8)
                    mom_data(12)=tot_durationmatrix_data(12)
                    mom_data(14)=tot_durationmatrix_data(16)                                ! 16 months survival
                    mom_data(16)=tot_durationmatrix_data(20)                       ! 20 months survival



                    mom_data(17)=u_data      !unemp_ave                 ! unemployment rate, average
                    mom_data(18)=rel_udur_move_stay_data                             ! rel length occupational movers/stayers
                    mom_data(19)=0.0
                    mom_data(19)=0.0

                    mom_data(20)=rtnoccten5yr_data
                    !mom_data(21)=cmy_cmp_data_ave                    !cmy_cmp_data2m
                    mom_data(22)=rtnoccten10yr_data
                    mom_data(23)=sepyoung_ave_data/sepprime_ave_data

                   mom_data(24)=uproportion_window_data
                   mom_data(25)=tot_corr_noccafternocc_data            !noccafternocc_ave_data
                   mom_data(26)=sep_ave_12m_posthire_data/sep_ave_12m_data
                   mom_data(27)=0.0
                   mom_data(28)=0.0


                    ! survival stats young
                    mom_data(29)=tot_durationmatrix_young_data(4)
                    mom_data(30)=tot_durationmatrix_young_data(8)
                    mom_data(31)=tot_durationmatrix_young_data(12)
                    mom_data(32)=tot_durationmatrix_young_data(16)
                    mom_data(33)=tot_durationmatrix_young_data(20)
                ! survival stats prime
                    mom_data(34)=tot_durationmatrix_prime_data(4)
                    mom_data(35)=tot_durationmatrix_prime_data(8)
                    mom_data(36)=tot_durationmatrix_prime_data(12)
                    mom_data(37)=tot_durationmatrix_prime_data(16)
                    mom_data(38)=tot_durationmatrix_prime_data(20)
                !! occmob duration profile young
                !    mom_data(39)=tot_durationmatrix_cr_cocc_young_data(2)
                !    mom_data(40)=tot_durationmatrix_cr_cocc_young_data(4)
                !    mom_data(41)=tot_durationmatrix_cr_cocc_young_data(8)
                !    mom_data(42)=tot_durationmatrix_cr_cocc_young_data(10)
                !    mom_data(43)=tot_durationmatrix_cr_cocc_young_data(12)
                !! occmob duration profile prime
                !    mom_data(44)=tot_durationmatrix_cr_cocc_prime_data(2)
                !    mom_data(45)=tot_durationmatrix_cr_cocc_prime_data(4)
                !    mom_data(46)=tot_durationmatrix_cr_cocc_prime_data(8)
                !    mom_data(47)=tot_durationmatrix_cr_cocc_prime_data(10)
                !    mom_data(48)=tot_durationmatrix_cr_cocc_prime_data(12)
                ! first two months
                   mom_data(49)=tot_durationmatrix_young_data(2)
                   mom_data(50)=tot_durationmatrix_prime_data(2)





                !! THIS HITS LEVELS AND SLOPES
                !    mom_data(53)=tot_durationmatrix_cr_cocc_p33_data(1)
                !    mom_data(54)=tot_durationmatrix_cr_cocc_p33_data(2)
                !    mom_data(55)=tot_durationmatrix_cr_cocc_p33_data(3)
                !    mom_data(56)=tot_durationmatrix_cr_cocc_p33_data(4)
                !    mom_data(57)=tot_durationmatrix_cr_cocc_p33_data(5)
                !    mom_data(58)=tot_durationmatrix_cr_cocc_p33_data(6)
                !    mom_data(59)=tot_durationmatrix_cr_cocc_p33_data(7)
                !    mom_data(60)=tot_durationmatrix_cr_cocc_p33_data(8)
                !
                !
                !    mom_data(61)=tot_durationmatrix_cr_cocc_p67_data(1)
                !    mom_data(62)=tot_durationmatrix_cr_cocc_p67_data(2)
                !    mom_data(63)=tot_durationmatrix_cr_cocc_p67_data(3)
                !    mom_data(64)=tot_durationmatrix_cr_cocc_p67_data(4)
                !    mom_data(65)=tot_durationmatrix_cr_cocc_p67_data(5)
                !    mom_data(66)=tot_durationmatrix_cr_cocc_p67_data(6)
                !    mom_data(67)=tot_durationmatrix_cr_cocc_p67_data(7)
                !    mom_data(68)=tot_durationmatrix_cr_cocc_p67_data(8)
                !    mom_data(69)=tot_durationmatrix_cr_cocc_p67_data(9)
                !    mom_data(70)=tot_durationmatrix_cr_cocc_p67_data(10)
                !    mom_data(71)=tot_durationmatrix_cr_cocc_p67_data(11)
                !    mom_data(72)=tot_durationmatrix_cr_cocc_p67_data(12)
                !


    !               !!=============================================
                    !!  SIMULATED MODEL DATA
                    !!============================================

                    mom=0.0_8

                    mom(1)=prod_ave                  ! average productivity
                    mom(2)=ac_prod
                    mom(3)=sqrt(var_prod)

                    mom(4)=elmatching_ave

                    !mom(5)=tot_durationmatrix_cocc(1)
                    !mom(7)=tot_durationmatrix_cocc(2)
                    !mom(9)=tot_durationmatrix_cocc(4)
                    !mom(11)=tot_durationmatrix_cocc(8)
                    !mom(13)=tot_durationmatrix_cocc(10)
                    !mom(15)=tot_durationmatrix_cocc(12)



                    mom(6)=tot_durationmatrix(2)
                    mom(8)=tot_durationmatrix(4)
                    mom(10)=tot_durationmatrix(8)
                    mom(12)=tot_durationmatrix(12)
                    mom(14)=tot_durationmatrix(16)                                ! 16 months survival
                    mom(16)=tot_durationmatrix(20)                       ! 20 months survival


                    mom(17)=unemp_earlier_e_ave      !unemp_ave                 ! unemployment rate, average
                    mom(18)=udur_mvec_occ_ave(1)/udur_mvec_nocc_ave(1) 
                    mom(19)=0.0

                    mom(20)=returnstenure5y
                    mom(21)=cmy_cmp_mom_ave
                    mom(22)=returnstenure10y
                    mom(23)=sepyoung_ave/sepprime_ave

                   mom(24)=uproportion_window
                   mom(25)=tot_corr_noccafternocc !noccafternocc_ave 
                   mom(26)=sep_ave_12m_posthire/sep_ave_12m

                   mom(27)=0.0
                   mom(28)=0.0

                                ! survival stats young
                                    mom(29)=tot_durationmatrix_young(4)
                                    mom(30)=tot_durationmatrix_young(8)
                                    mom(31)=tot_durationmatrix_young(12)
                                    mom(32)=tot_durationmatrix_young(16)
                                    mom(33)=tot_durationmatrix_young(20)
                                ! survival stats prime
                                    mom(34)=tot_durationmatrix_prime(4)
                                    mom(35)=tot_durationmatrix_prime(8)
                                    mom(36)=tot_durationmatrix_prime(12)
                                    mom(37)=tot_durationmatrix_prime(16)
                                    mom(38)=tot_durationmatrix_prime(20)
                                !! occmob duration profile young
                                !    mom(39)=tot_durationmatrix_cr_cocc_young(2)
                                !    mom(40)=tot_durationmatrix_cr_cocc_young(4)
                                !    mom(41)=tot_durationmatrix_cr_cocc_young(8)
                                !    mom(42)=tot_durationmatrix_cr_cocc_young(10)
                                !    mom(43)=tot_durationmatrix_cr_cocc_young(12)
                                !! occmob duration profile prime
                                !    mom(44)=tot_durationmatrix_cr_cocc_prime(2)
                                !    mom(45)=tot_durationmatrix_cr_cocc_prime(4)
                                !    mom(46)=tot_durationmatrix_cr_cocc_prime(8)
                                !    mom(47)=tot_durationmatrix_cr_cocc_prime(10)
                                !    mom(48)=tot_durationmatrix_cr_cocc_prime(12)
                                    ! shortduration
                                    mom(49)=tot_durationmatrix_young(2)
                                    mom(50)=tot_durationmatrix_prime(2)

                                mom(27)=0.0
                                mom(28)=0.0







                    !mom(53)=tot_durationmatrix_cr_cocc_p33(1)
                    !mom(54)=tot_durationmatrix_cr_cocc_p33(2)
                    !mom(55)=tot_durationmatrix_cr_cocc_p33(3)
                    !mom(56)=tot_durationmatrix_cr_cocc_p33(4)
                    !mom(57)=tot_durationmatrix_cr_cocc_p33(5)
                    !mom(58)=tot_durationmatrix_cr_cocc_p33(6)
                    !mom(59)=tot_durationmatrix_cr_cocc_p33(7)
                    !mom(60)=tot_durationmatrix_cr_cocc_p33(8)
                    !
                    !
                    !mom(61)=tot_durationmatrix_cr_cocc_p67(1)
                    !mom(62)=tot_durationmatrix_cr_cocc_p67(2)
                    !mom(63)=tot_durationmatrix_cr_cocc_p67(3)
                    !mom(64)=tot_durationmatrix_cr_cocc_p67(4)
                    !mom(65)=tot_durationmatrix_cr_cocc_p67(5)
                    !mom(66)=tot_durationmatrix_cr_cocc_p67(6)
                    !mom(67)=tot_durationmatrix_cr_cocc_p67(7)
                    !mom(68)=tot_durationmatrix_cr_cocc_p67(8)
                    !mom(69)=tot_durationmatrix_cr_cocc_p67(9)
                    !mom(70)=tot_durationmatrix_cr_cocc_p67(10)
                    !mom(71)=tot_durationmatrix_cr_cocc_p67(11)
                    !mom(72)=tot_durationmatrix_cr_cocc_p67(12)


   !================================
   ! distance calculation
   !=================================

    ! THE WEIGHT MATRIX IS SET MANUALLY TO REFLECT TWO OBJECTIVES, IN ORDER OF DECREASING IMPORTANCE
                        ! (1) THE MORE IMPORTANT MOMENTS, GIVEN THE THEORY, ARE WEIGHTED MORE
                        ! (2) IF A HIGHER WEIGHT SPEEDS UP THE INITIAL CONVERGENCE TO REASONABLE FITS, WITHOUT A LARGE IMPACT ON (1)



                        omega=0.0_8
                        DO i=1,50
                            omega(i,i)=1.0_8            !/(mom_data(i)**2)
                        END DO


                        !===================
                        ! MOMENT WEIGHTS
                        !===================
                        omega(1,1)=2000.0_8*omega(1,1) ! ave_p
                        omega(2,2)=2000.0_8*omega(2,2) ! rho_p

                        omega(3,3)=1000.0_8*omega(3,3) ! sigma_p

                        omega(4,4)=20.0_8*omega(4,4)  ! empirical eta

                        omega(5,5)=2000.0_8*omega(5,5)        ! 1m occmob
                        omega(7,7)=240.0_8*omega(7,7)        ! 2m occmob
                        omega(9,9)=240.0_8*omega(9,9)        ! 4m occmob
                        omega(11,11)=300.0_8*omega(11,11)    ! 8m occmob
                        omega(13,13)=300.0_8*omega(13,13)    ! 10m occmob
                        omega(15,15)=300.0_8*omega(15,15)    ! 12m occmob



                        ! job finding rates
                        omega(6,6)=100.0_8*omega(6,6)        ! 2m surv
                        omega(8,8)=100.0_8*omega(8,8)       ! 4m surv
                        omega(10,10)=30.0_8*omega(10,10)    ! 8m surv
                        omega(12,12)=10.0_8*omega(12,12)    ! 12m surv
                        omega(14,14)=20.0_8*omega(14,14)    ! 16m surv
                        omega(16,16)=20.0_8*omega(16,16)    ! 20m surv



                    ! omega(8,8)=1.0_8
                        omega(17,17)=500.0_8*omega(17,17) ! unempl_Ave
                        omega(18,18)=1000.0_8*omega(18,18)   ! rel dur
                        omega(19,19)=0.0_8*omega(19,19)    ! rel uy/up
                        ! tenure
                    omega(20,20)=250.0_8*omega(20,20)
                    omega(22,22)=250.0_8*omega(22,22)
                        ! cy/cp
                    omega(21,21)=10000.0_8*omega(21,21)        ! cy/cp
                        ! sepy/sepp
                    omega(23,23)=100.0_8*omega(23,23)


                        ! u prop
                    omega(24,24)=20.0_8*omega(24,24)

                    ! repeat mobility
                    omega(25,25)=2000.0_8*omega(25,25)
                        ! repeat separation moments
                        omega(26,26)=50.0_8*omega(26,26)


                    DO i=29,50
                            omega(i,i)=60.0_8            !/(mom_data(i)**2)
                    END DO

                    ! 4m survival
                    omega(29,29)=6.0*omega(29,29)
                    omega(34,34)=6.0*omega(34,34)


                    ! 16m survival
                    omega(32,32)=0.50_8*omega(32,32)
                    omega(37,37)=0.50_8*omega(37,37)

                    ! 20m survival
                    omega(33,33)=0.30_8*omega(33,33)
                    omega(38,38)=0.30_8*omega(38,38)

                    ! age difference in initial survival (2m)
                    omega(49,49)=15.00_8*omega(49,49)
                    omega(50,50)=15.00_8*omega(50,50)

                    ! duration profile with age
                    DO i=39,48
                            omega(i,i)=500.0_8            !/(mom_data(i)**2)
                    END DO



                    omega(51,51)=0.0_8
                    omega(52,52)=0.0_8



                   DO i=53,56
                        omega(i,i)=600.0_8            !/(mom_data(i)**2)
                   END DO

                   DO i=57,60
                        omega(i,i)=1000.0_8            !/(mom_data(i)**2)
                   END DO


                   DO i=61,72
                        omega(i,i)=600.0_8            !/(mom_data(i)**2)
                   END DO

                   DO i=67,72
                        omega(i,i)=1000.0_8            !/(mom_data(i)**2)
                    END DO

    
    
    
    
    !!!===========
    !!!  NO REALLOCATION: REWEIGHTING THE SURVIVAL PROFILE
    !!!===========
    !    omega(6,6)=300.0_8        ! 2m surv
    !    omega(8,8)=300.0_8       ! 4m surv
    !    omega(10,10)=100.0_8    ! 8m surv
    !    omega(12,12)=100.0_8    ! 12m surv
    !    omega(14,14)=80.0_8    ! 16m surv
    !    omega(16,16)=50.0_8    ! 20m surv
    !
    !    ! FOR YOUNG
    !    omega(49,49)=300.0_8        ! 2m surv
    !    omega(29,29)=300.0_8        ! 4m surv
    !    omega(30,30)=100.0_8       ! 8m surv
    !    omega(31,31)=100.0_8    ! 12m surv
    !    omega(32,32)=80.0_8    ! 16m surv
    !    omega(33,33)=50.0_8    ! 20m surv
    !    ! FOR PRIME
    !    omega(50,50)=300.0_8        ! 2m surv
    !    omega(34,34)=300.0_8        ! 4m surv
    !    omega(35,35)=100.0_8       ! 8m surv
    !    omega(36,36)=100.0_8    ! 12m surv
    !    omega(37,37)=80.0_8    ! 16m surv
    !    omega(38,38)=50.0_8    ! 20m surv
    !
    
   !====================================================
   ! CALCULATE MOMENT DISTANCE, with diagonal weighting matrix
   !====================================================

        ! AVERAGE PRODUCTIVITY: [0, infty)
        tempint=1
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF

        ! MOMENTS 2-19: ALL BETWEEN 0 and 1
            DO tempint=2,17
                                 IF(mom(tempint)>=0.0 .AND. mom(tempint)<=1.0 .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
                                ELSE
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
                                END IF
            END DO

        ! RETURNS TO TENURE [0, infty), SEPARATION AGE DIFFERENCE, REPEAT SEP RATIO, DISPERSION: [0, infty)
    tempint=18
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8 .AND. &
               mom(tempint)*mom_data(tempint)>0.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF

    tempint=19
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF
    
    ! returns to occ. tenure can in
    tempint=20
            IF(mom(tempint)>-HUGE(1.0) .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(MAX(mom(tempint),0.0_8)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF

    tempint=22
            IF(mom(tempint)>-HUGE(1.0) .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(MAX(mom(tempint),0.0_8)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF


    tempint=21
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF
    tempint=23
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF

    ! MOMENTS 24-25: ALL BETWEEN 0 and 1
            DO tempint=24,25
                                 IF(mom(tempint)>=0.0 .AND. mom(tempint)<=1.0 .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
                                ELSE
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
                                END IF
            END DO

        tempint=26
            IF(mom(tempint)>=0.0 .AND. mom(tempint)<HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
            ELSE
                                        omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
            END IF


    ! MOMENTS 27-28 - OPEN : ALL BETWEEN 0 and +infty
            DO tempint=27,28
                                 IF(mom(tempint)>=0.0 .AND. mom(tempint)<=HUGE(1.0_8) .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
                                ELSE
                                    omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
                                END IF
            END DO




                DO tempint=29,50
                                            IF(mom(tempint)>=0.0 .AND. mom(tempint)<=1.0 .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                                omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
                                            ELSE
                                                omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
                                            END IF
                END DO


    
                DO tempint=53,72
                                            IF(mom(tempint)>=0.0 .AND. mom(tempint)<=1.0 .AND. mom(tempint)==mom(tempint) .AND. mom(tempint) .ne. mom(tempint)+1.0_8) THEN
                                                omega(tempint,tempint)=omega(tempint,tempint)*(10.0*(mom(tempint)-mom_data(tempint))/(mom(tempint)+mom_data(tempint)))**2
                                            ELSE
                                                omega(tempint,tempint)=omega(tempint,tempint)*(10.0)**2
                                            END IF
                END DO
    
    
    !!====================================
    !!  ADD DISTANCES
    !!======================================

   tempreal=0.0_8
   DO i=1, nmom_local !nmom_local
       IF((omega(i,i) .ne. omega(i,i)+1.0_8) .AND. omega(i,i)>0.0_8) THEN
            tempreal=tempreal+omega(i,i)
       ELSE IF (omega(i,i) == omega(i,i)+1.0_8) THEN
            tempreal=tempreal+100000.0_8
       END IF
    END DO
    mom1_dist=tempreal



    !================================================
    !================================================
    !
    !  R E P O R T I N G
    !
    !=================================================
    !==================================================





! REPORT PARAMETERS FOR PAPER    
OPEN(UNIT=56, file='parameters.txt', form='formatted', status='replace')


   WRITE(56, FMT='((A30, F15.9))') 'reallocation cost c:', reallc
   WRITE(56, FMT='((A30, F15.9))') 'vacancy cost k:', k
   WRITE(56, FMT='((A30, F15.9))') 'unemp flow benefit:', b
   WRITE(56, FMT='((A30, F15.9))') 'eta:', eta
   WRITE(56, FMT='((A30, F15.9))') 'delta_lowhc:', deltalowhc
   WRITE(56, FMT='((A30, F15.9))') 'delta_highhc:', deltahighhc
   WRITE(56, FMT='((A30, F15.9))') 'z_correction:', zcorrection
   WRITE(56, FMT='((A30, F15.9))') 'rho_p:', rho_p
   WRITE(56, FMT='((A30, F15.9))') 'sigma_p:', sigma_p
   !WRITE(56, FMT='((A30, F15.9))') 'rho_z:', reallc
   !WRITE(56, FMT='((A30, F15.9))') 'sigma_z:', reallc
   WRITE(56, FMT='((A30, F15.9))') 'x2 hc level:', xvector(2)
   WRITE(56, FMT='((A30, F15.9))') 'x3 hc level:', xvector(3)
   WRITE(56, FMT='((A30, F15.9))') 'skill depreciation:', skilldep

CLOSE(56)


OPEN(UNIT=13, file='moments.txt', form='formatted', status='replace')

        write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'average productivity:',   mom(1), ' , ', mom_data(1)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'rhop_p:',   mom(2), ' , ', mom_data(2) 
        write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'sigma_p:', mom(3), ' , ', mom_data(3) 
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'ave unemp (earlier empl):', mom(17), ' , ', mom_data(17)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'repeat mob stay|prev.stay:', mom(25), ' , ', mom_data(25)
        !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'rel occ mob young/prime', mom(21), ' , ', mom_data(21)
        write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'sep rate young/prime:', mom(23), ' , ', mom_data(23)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'rel. sep rate hire/empl', mom(26), ' , ',mom_data(26)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'prob u within 3yrs for empl.', mom(24), ' , ', mom_data(24)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'emp. elas matching fcn:', mom(4), ' , ', mom_data(4) 
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'emp return tenure, 5yr:', mom(20), ' , ', mom_data(20)
        write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'emp return tenure, 10yr:', mom(22), ' , ', mom_data(22)
        !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'rel u.dur movers/stayers:', mom(18), ' , ', mom_data(18)
        
        !write(13,*) '' 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 1:', mom(5), ' , ', mom_data(5) 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 2:', mom(7), ' , ', mom_data(7) 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 4:', mom(9), ' , ', mom_data(9) 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 8:', mom(11), ' , ', mom_data(11) 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 10:', mom(13), ' , ', mom_data(13) 
        !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob, duration >= 12:', mom(15), ' , ', mom_data(15) 
        
        write(13,*) '' 
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 2m:', mom(6), ' , ', mom_data(6)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 4m:', mom(8), ' , ', mom_data(8)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 8m:', mom(10), ' , ', mom_data(10)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 12:', mom(12), ' , ', mom_data(12)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 16:', mom(14), ' , ', mom_data(14)
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival profile 20:', mom(16), ' , ', mom_data(16)

        


    
        !WRITE(13, *) 'lifecycle survival profile'
        write(13,*) '' 
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  2m:', mom(49), ' , ',mom_data(49)
        WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  4 :', mom(29), ' , ', mom_data(29)
        WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  8 :', mom(30), ' , ', mom_data(30)
        WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  12:', mom(31), ' , ', mom_data(31)
        WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  16:', mom(32), ' , ', mom_data(32)
        WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival young  20:', mom(33), ' , ', mom_data(33)
        
        write(13,*) '' 
        write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  2m:', mom(50), ' , ',mom_data(50)
        WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  4m:', mom(34), ' , ',mom_data(34)
        WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  8m:', mom(35), ' , ',mom_data(35)
        WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  12:', mom(36), ' , ',mom_data(36)
        WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  16:', mom(37), ' , ',mom_data(37)
        WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'u.survival prime  20:', mom(38), ' , ',mom_data(38)
    
       ! write(13,*) '' 
       !WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob young w. dur 2 :', mom(39), ' , ', mom_data(39)
       !WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob young w. dur 4 :', mom(40), ' , ', mom_data(40)
       !WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob young w. dur 8 :', mom(41), ' , ', mom_data(41)
       !WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob young w. dur 10:', mom(42), ' , ', mom_data(42)
       !WRITE(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob young w. dur 12:', mom(43), ' , ', mom_data(43)
       !
       !write(13,*) '' 
       !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob prime w. dur 2 :', mom(44), ' , ',mom_data(44)
       !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob prime w. dur 4 :', mom(45), ' , ',mom_data(45)
       !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob prime w. dur 8 :', mom(46), ' , ',mom_data(46)
       !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob prime w. dur 10:', mom(47), ' , ',mom_data(47)
       !WRITE(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob prime w. dur 12:', mom(48), ' , ',mom_data(48)
    !   !
    !   !
    !!write(13,*) ' good times'
    !write(13,*) '' 
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 1 good times:', mom(53), ' , ', mom_data(53)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 2 good times:', mom(54), ' , ',mom_data(54)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 3 good times:', mom(55), ' , ', mom_data(55)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 4 good times:', mom(56), ' , ',mom_data(56)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 5 good times:', mom(57), ' , ', mom_data(57)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 6 good times:', mom(58), ' , ',mom_data(58)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 7 good times:', mom(59), ' , ', mom_data(59)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 8 good times:', mom(60), ' , ',mom_data(60)
    !
    !write(13,*) ''
    !!write(13,*) ' bad times '
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 1 bad times:', mom(61), ' , ', mom_data(61)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 2 bad times:', mom(62), ' , ',mom_data(62)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 3 bad times:', mom(63), ' , ', mom_data(63)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 4 bad times:', mom(64), ' , ',mom_data(64)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 5 bad times:', mom(65), ' , ', mom_data(65)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 6 bad times:', mom(66), ' , ',mom_data(66)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 7 bad times:', mom(67), ' , ', mom_data(67)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 8 bad times:', mom(68), ' , ',mom_data(68)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 9 bad times:', mom(69), ' , ', mom_data(69)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 10 bad times:', mom(70), ' , ',mom_data(70)
    !write(13,FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 11 bad times:', mom(71), ' , ', mom_data(71)
    !write(13, FMT='(1(A25, F8.4, A4, F8.4, A4 ))'), 'occmob dur 12 bad times:', mom(72), ' , ',mom_data(72)

CLOSE(13)


OPEN(UNIT=13, file='ts_xcorrelations.txt', form='formatted', status='replace')



       WRITE(13, *) ' '
       WRITE(13, *) ' '
       WRITE(13, *) '======= '
       WRITE(13, *) ' CROSSCORRELATION TABLE **SMOOTHED** HPF-LOGGED '
       WRITE(13, *) '======== '

WRITE(13,*) ' '
WRITE(13,*) '==================================================='
WRITE(13, FMT='(A8,9(A9))')"statistc", "u", "v", "tghtness", "sep",  "jb find", "prod", "uall", "reall", "reall_chk"
WRITE(13, FMT='(A8,9(F9.2))') "stdev", sqrt(var_u5) , sqrt(var_v5)  , sqrt(var_theta5)  , sqrt(var_sep5) , sqrt(var_jf5) , &
sqrt(var_prod5) , sqrt(var_uall5) , sqrt(var_reall5), sqrt(var_reall5_2)
WRITE(13, FMT='(A8,8(F9.2))') "autocorr", ac_u5 , ac_v5  , ac_theta5  , ac_sep5 , ac_jf5 , &
ac_prod5 , ac_uall5 , ac_reall5
WRITE(13,*) '---------------------------------------------------'
WRITE(13, FMT='(A8,8(F9.2))') "u", var_u5 , corr_uv5  , corr_utheta5  , corr_usep5  , corr_ujf5  ,  &
corr_uprod5  , corr_uallu5  , corr_ureall5
WRITE(13, FMT='(A8,9X, 7(F9.2))') "v", var_v5  , corr_vtheta5  , corr_vsep5  , corr_vjf5  ,  &
corr_vprod5  , corr_uallv5  , corr_vreall5
WRITE(13, FMT='(A8,18X, 6(F9.2))') "theta", var_theta5, corr_thetasep5  , corr_thetajf5  ,  &
corr_thetaprod5  , corr_ualltheta5  , corr_thetareall5
WRITE(13, FMT='(A8,27X, 5(F9.2))') "sep",  var_sep5 , corr_sepjf5  ,  &
corr_sepprod5  , corr_uallsep5  , corr_sepreall5
WRITE(13, FMT='(A8,36X, 4(F9.2))') "jf",  var_jf5,  &
corr_jfprod5 , corr_ualljf5  , corr_jfreall5
WRITE(13, FMT='(A8,45X, 3(F9.2))') "prod5", var_prod5, corr_uallprod5  , corr_prodreall5
WRITE(13, FMT='(A8,54X, 2(F9.2))') "uall", var_uall5 , corr_uallreall5
WRITE(13, FMT='(A8,63X, (F9.2))') "reall", var_reall5
WRITE(13,*) '--------------------------------------------------'



CLOSE(13)


OPEN(UNIT=13, file='ts_xcorrelations_unsmoothed.txt', form='formatted', status='replace')


       WRITE(13, *) ' '
       WRITE(13, *) ' '
       WRITE(13, *) '====================== '
       WRITE(13, *) ' UNSMOOTHED HPF-LOGGED '
       WRITE(13, *) '====================== '

WRITE(13,*) ' '
WRITE(13,*) '==================================================='
WRITE(13, FMT='(A8,8(A9))')"statistc", "u", "v", "tghtness", "sep",  "jb find", "prod", "uall", "reall"
WRITE(13, FMT='(A8,8(F9.2))') "stdev", sqrt(var_u) , sqrt(var_v)  , sqrt(var_theta)  , sqrt(var_sep) , sqrt(var_jf) , &
sqrt(var_prod) , sqrt(var_uall) , sqrt(var_reall)
WRITE(13, FMT='(A8,8(F9.2))') "autocorr", ac_u , ac_v  , ac_theta  , ac_sep , ac_jf , &
ac_prod , ac_uall , ac_reall
WRITE(13,*) '---------------------------------------------------'
WRITE(13, FMT='(A8,8(F9.2))') "u", var_u , corr_uv  , corr_utheta  , corr_usep  , corr_ujf  ,  &
corr_uprod  , corr_uallu  , corr_ureall
WRITE(13, FMT='(A8,9X, 7(F9.2))') "v", var_v  , corr_vtheta  , corr_vsep  , corr_vjf  ,  &
corr_vprod  , corr_uallv  , corr_vreall
WRITE(13, FMT='(A8,18X, 6(F9.2))') "theta", var_theta, corr_thetasep  , corr_thetajf  ,  &
corr_thetaprod  , corr_ualltheta  , corr_thetareall
WRITE(13, FMT='(A8,27X, 5(F9.2))') "sep",  var_sep , corr_sepjf  ,  &
corr_sepprod  , corr_uallsep , corr_sepreall
WRITE(13, FMT='(A8,36X, 4(F9.2))') "jf",  var_jf,  corr_jfprod , corr_ualljf  , corr_jfreall
WRITE(13, FMT='(A8,45X, 3(F9.2))') "prod", var_prod, corr_uallprod  , corr_prodreall
WRITE(13, FMT='(A8,54X, 2(F9.2))') "uall", var_uall , corr_uallreall
WRITE(13, FMT='(A8,63X, (F9.2))') "reall", var_reall
WRITE(13,*) '--------------------------------------------------'

CLOSE(13)

OPEN(UNIT=13, file='incompl_durdistr_stats.txt', form='formatted', status='replace')
WRITE(13,*) ' '
WRITE(13,*) ' '
WRITE(13,*) '~~~~ INCOMPL U DURATION DISTRIBUTION FROM SIPP, ALL (MODEL -- DATA)'
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) ' < 3months', uprop_3m_ave, '--', uprop_3m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) '< 5months', uprop_5m_ave, '--', uprop_5m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) '5-9 months', uprop_9m_ave, '--', uprop_9m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) '9-13months ', uprop_13m_ave, '--', uprop_13m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) '13-18months ', uprop_g13m_ave, '--', uprop_g13m_ave_data
WRITE(13,*) '~~~~ INCOMPL U DISTRIBUTION: **YNG VS PRM** (MODEL -- DATA)' 
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) 'prop 1-2m;    yng:', uprop_yng_3m_ave, '--', uprop_yng_3m_ave_data,   ' prm: ', uprop_prm_3m_ave, '--', uprop_prm_3m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) 'prop 1-4m;    yng:', uprop_yng_5m_ave, '--', uprop_yng_5m_ave_data,   ' prm: ', uprop_prm_5m_ave, '--', uprop_prm_5m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) 'prop 5-8m;    yng:', uprop_yng_9m_ave, '--', uprop_yng_9m_ave_data,   ' prm: ', uprop_prm_9m_ave, '--', uprop_prm_9m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) 'prop 9-12m;   yng:', uprop_yng_13m_ave, '--', uprop_yng_13m_ave_data, ' prm: ', uprop_prm_13m_ave, '--', uprop_prm_13m_ave_data
WRITE(13,FMT='(2(A20, F5.2, A2, F5.2))' ) 'prop >=13m;   yng:', uprop_yng_g13m_ave, '--',uprop_yng_g13m_ave_data,' prm: ', uprop_prm_g13m_ave, '--', uprop_prm_g13m_ave_data

WRITE(13,*) ' '
WRITE(13,*) ' '
WRITE(13,*) ' - HP-FILT. SEMI-ELASTICITY OF DURATION DIST TO UNEMPLOYMENT RATE-'
!WRITE(13,*) 'prop u<3m  :', hp_uprop3m_unemp_semielasticity, '--', hp_uprop3m_unemp_semielasticity_instdata
!WRITE(13,*) 'prop u<5m  :', hp_uprop5m_unemp_semielasticity, '--', hp_uprop5m_unemp_semielasticity_instdata
!WRITE(13,*) 'prop u5-9  :', hp_uprop9m_unemp_semielasticity, '--', hp_uprop9m_unemp_semielasticity_instdata
!WRITE(13,*) 'prop u9-13 :', hp_uprop13m_unemp_semielasticity, '--', hp_uprop13m_unemp_semielasticity_instdata
!WRITE(13,*) 'prop u13-8 :', hp_upropg13m_unemp_semielasticity, '--', hp_upropg13m_unemp_semielasticity_instdata
WRITE(13,*) 'prop u<3m  :', sm_hp_uprop3m_u_semi_el, '--', hp_uprop3m_unemp_semielasticity_instdata
WRITE(13,*) 'prop u<5m  :', sm_hp_uprop5m_u_semi_el, '--', hp_uprop5m_unemp_semielasticity_instdata
WRITE(13,*) 'prop u5-9  :', sm_hp_uprop9m_u_semi_el,  '--', hp_uprop9m_unemp_semielasticity_instdata
WRITE(13,*) 'prop u9-13 :', sm_hp_uprop13m_u_semi_el, '--', hp_uprop13m_unemp_semielasticity_instdata
WRITE(13,*) 'prop u13-8 :', sm_hp_upropg13m_u_semi_el,  '--', hp_upropg13m_unemp_semielasticity_instdata

WRITE(13,*) '  -note: NaN could occur if empty quarterly obs at high duration in simulation, '
WRITE(13,*) '              increase nsim_gen in mod_global_ctv.f90                               '
WRITE(13,*) ' '
WRITE(13,*) ' '
WRITE(13,*) ' - (LIN. DETRENDED, NO HPF) ELASTICITY DURATION DIST TO UNEMPLOYMENT RATE-'
write(13,*) 'prop u<3m  :', u3m_unemp_elasticity, '--', u3m_unemp_elasticity_data
write(13,*) 'prop u<5m  :', u5m_unemp_elasticity, '--', u5m_unemp_elasticity_data
write(13,*) 'prop u5-9m :', u9m_unemp_elasticity, '--', u9m_unemp_elasticity_data
write(13,*) 'prop u9-13m:', u13m_unemp_elasticity, '--', u13m_unemp_elasticity_data
write(13,*) 'prop >13m  :', ug13m_unemp_elasticity, '--', ug13m_unemp_elasticity_data
WRITE(13,*) '  -note: NaN could occur if empty quarterly obs at high duration in simulation, '
WRITE(13,*) '              increase nsim_gen in mod_global_ctv.f90                               '
WRITE(13,*) ' '
write(13, *) ' SEMI ELAST AVE U DURATION OF MOVERS/STAYERS WITH U.RATE (LIN DETRENDED)'
write(13, FMT='(A24,A10, A2, A10 , A2, A10, A2, A10)')      'MEASURE', '1-18mth sp', '--', 'data 1-18', ',,', 'all spells', ',', 'data all'
write(13, FMT='(A24,F10.3, A2, F10.3 , A2,E10.2, A2, E10.2)') 'OCC MOVERS :', udur_occ_with_lu, '--', udur_occ_with_lu_data
write(13, FMT='(A24,F10.3, A2, F10.3 , A2,E10.2, A2, E10.2)') 'OCC STAYERS:', udur_nocc_with_lu, '--', udur_nocc_with_lu_data
WRITE(13,*) ' '
WRITE(13,*) ' '
write(13, *) ' SEMI ELAST AVE U DURATION OF MOVERS/STAYERS WITH U.RATE (HP FILTERED)'
write(13, FMT='(A24,F10.3, A2, F10.3 , A2,E10.2, A2, E10.2)') 'OCC MOVERS :', udur_occ_with_hplu, '--', udur_occ_with_hplu_data
write(13, FMT='(A24,F10.3, A2, F10.3 , A2,E10.2, A2, E10.2)') 'OCC STAYERS:', udur_nocc_with_hplu, '--', udur_nocc_with_hplu_data
        
    
CLOSE(13)

OPEN(UNIT=13, file='further_stats.txt', form='formatted', status='replace')

WRITE(13,*) ' '
    
    
    !WRITE(13, *) 'FOOTNOTE 22 SEPARATION cond. on move/stayer previous u spell (MODEL--DATA)'
    !WRITE(13,FMT='(A27, F9.4, A5, F9.4, A5, F9.4)') 'ave mthly sep after move:', sepocc_ave, ' -- ', sepocc_ave_data
    !WRITE(13,FMT='(A27, F9.4, A5, F9.4, A5, F9.4)') 'ave mthly sep after stay:', sepnocc_ave, ' -- ', sepnocc_ave_data
    
    WRITE(13, *) '' 
    WRITE(13, *) 'STRONGER RELATIVE RESPONSE TO SEPARATIONS OF PRIME TO YOUNG, MODEL'
    WRITE(13, *) '   (elasticity hp-filtered sep with hp-filtered productivity) '
    WRITE(13,*) 'sep young:', sep_young_prod_elasticity 
    WRITE(13,*) 'sep prime:', sep_prime_prod_elasticity 

    WRITE(13, *) '' 
    WRITE(13, *) 'SLIGHTLY STRONGER REL. RESPONSE U RATE OF PRIME TO YOUNG, MODEL'
    WRITE(13, *) '   (elasticity hp-filtered u with hp-filtered productivity) '
    WRITE(13,*) 'unempl young:', u_young_prod_elasticity
    WRITE(13,*) 'unempl prime:', u_prime_prod_elasticity

    WRITE(13, *) ''
    WRITE(13, *) 'mM ratio in the model'
    
    WRITE(13, FMT='(A40, X, F11.4)') 'min-Mean, all wages:                    ', m0mratio_all
    WRITE(13, FMT='(A40, X, F11.4)') 'min-Mean, controlling for age:          ', m0mratio_age_contr
    WRITE(13, FMT='(A40, X, F11.4)') 'min-Mean, controlling for age and cycle:', m0mratio_age_p_contr
    
    WRITE(13, FMT='(A40, X, F11.4)') '1st ptile-Mean, all wages:                    ', m1mratio_all
    WRITE(13, FMT='(A40, X, F11.4)') '1st ptile-Mean, controlling for age:          ', m1mratio_age_contr
    WRITE(13, FMT='(A40, X, F11.4)') '1st ptile-Mean, controlling for age and cycle:', m1mratio_age_p_contr
    
    WRITE(13, *) 'Preferred measure: min-Mean, controls age, cycle' 
    
    
    WRITE(13, *) ''
    WRITE(13, *) 'model generates that u is more concentrated for prime-aged'
    WRITE(13,FMT='(A27, F9.4, A5, F9.4, A5, F9.4)') '~~~~u prop window ~~~~ ', uproportion_window, ' -- ', uproportion_window_data
    WRITE(13,FMT='(A27, F9.4, A5, F9.4, A5, F9.4)') '~~~~uprop window yng ~ ', uproportion_window_y, ' --', uproportion_window_y_data
    WRITE(13,FMT='(A27, F9.4, A5, F9.4, A5, F9.4)') '~~~~uprop window prm ~ ', uproportion_window_p, ' -- ', uproportion_window_p_data



CLOSE(13)

!! WEIGHTS FILE 

filename='weights'//trim(file_id)//trim(x1)//'.csv'
OPEN(UNIT=12, file=filename, form='formatted', status='replace')

DO pcnt=1, nmom_local
            write(12, fmt='(A6,I3,A1,I3,A2)', advance='no') 'omega(',pcnt, ',', pcnt, ')='
            write(12, fmt='(F12.6)') momweightl(pcnt)
END DO

CLOSE(12)

!! PROFILES FILE

filename='profile'//trim(file_id)//trim(x1)//'.csv'
OPEN(UNIT=12, file=filename, form='formatted', status='replace')

WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'month', ','
!! CORRECTED MOBILITY
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdury_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdury_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdurp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'cmdurp_d', ','
!! RAW MOBILITY
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdury_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdury_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdurp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mdurp_d', ','
!! CYCLICAL MOB
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badcmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badcmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdcmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdcmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdmdur_d', ','
    !! spells in distribution in good and bad times
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badnspel_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badnspel_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdnspel_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdnspel_d', ','
    !! conf interval
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badcm_lb_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'badcm_ub_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdcm_lb_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'gdcm_ub_d', ','
!! SURVIVAL
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'surv_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'surv_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'survy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'survy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'survp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'survp_d', ','
!! stayers survival
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurv_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurv_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurvy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurvy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurvp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'stsurvp_d', ','
!! movers survival
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurv_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurv_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurvy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurvy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurvp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvsurvp_d', ','
!! HAZARDS
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'haz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'haz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'hazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'hazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'hazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'hazp_d', ','
!! stayers haz
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthaz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthaz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'sthazp_d', ','
!! movers haz
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhaz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhaz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') 'mvhazp_d', ','
!! separations after new hire
WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') 'sepallyr_m', ',','sepallyr_d', ','
WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') 'seprhire_m', ',','seprhire_d', ','
WRITE(12,FMT='(A10,A1)') '-----', ','


DO tcnt=1, 18
WRITE(12,FMT='(I10,A1)', ADVANCE='NO') tcnt, ','
!! CORRECTED MOBILITY
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_prime_data(tcnt), ','
!! RAW MOBILITY
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_prime_data(tcnt), ','
!! CYCLICAL MOB
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_p67(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_p67_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p67(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p67_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_p33(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cr_cocc_p33_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p33(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p33_data(tcnt), ','
    !! spells in distribution in good and bad times
IF (tcnt==1) THEN 
    WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0, ','
ELSE 
    WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_nocc_p67(tcnt), ','
END IF 

WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_durationmatrix_nocc_p67_data(tcnt), ','

IF (tcnt==1) THEN 
    WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0, ','
ELSE 
    WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_nocc_p33(tcnt), ','
END IF 

WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') (1.0-tot_durationmatrix_nocc_p33_data(tcnt)), ','

!! conf interval
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p67_lb_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p67_ub_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p33_lb_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_cocc_p33_ub_data(tcnt), ','

!! SURVIVAL
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_prime_data(tcnt), ','
!! stayers survival
tempint=1!! adjustment
!tot_durationmatrix2_nocc_young_data
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_young_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_prime_data(tcnt-tempint), ','
!! movers survival
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_young_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_prime_data(tcnt-tempint), ','
!! HAZARDS
 !WRITE(13,FMT='(I7,X,6(A3, F7.3))') tcnt-1, ' a:',1.0-tot_haz_durationmatrix(tcnt), ' y:', 1.0-tot_haz_durationmatrix_young(tcnt),' p:', 1.0-tot_haz_durationmatrix_prime(tcnt), &
 !                                   '|a:', tot_haz_durationmatrix_data(tcnt),' y:', tot_haz_durationmatrix_young_data(tcnt),' p:', tot_haz_durationmatrix_prime_data(tcnt)

WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_durationmatrix(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_durationmatrix_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_durationmatrix_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_durationmatrix_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_durationmatrix_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_durationmatrix_prime_data(tcnt), ','
!! stayers haz
!WRITE(13,FMT='(I7,X,6(A3, F7.3))') tcnt-1, ' a:',1.0-tot_haz_udurationmatrix_m(tcnt), ' y:', 1.0-tot_haz_udurationmatrix_m_young(tcnt),' p:', 1.0-tot_haz_udurationmatrix_m_prime(tcnt), &
!                                    '|a:', tot_haz_udurationmatrix_m_data(tcnt),' y:', tot_haz_udurationmatrix_m_young_data(tcnt),' p:', tot_haz_udurationmatrix_m_prime_data(tcnt)
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_s(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_s_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_s_young(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_s_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_s_prime(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_s_prime_data(tcnt), ','
!! movers haz
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_m(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_m_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_m_young(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_m_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 1.0-tot_haz_udurationmatrix_m_prime(tcnt+1), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_haz_udurationmatrix_m_prime_data(tcnt), ','
!! separations after new hire
!WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 'sepallyr', ','
!WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') 'seprhireyr', ','
   IF(tcnt==1) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_1m, ',', sep_ave_1m_data, ','
   IF(tcnt==2) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_2m, ',', sep_ave_2m_data, ','
   IF(tcnt==3) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_3m, ',', sep_ave_3m_data, ','
   IF(tcnt==4) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_4m, ',', sep_ave_4m_data, ','
   IF(tcnt==5) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_5m, ',', sep_ave_5m_data, ','
   IF(tcnt==6) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_6m, ',', sep_ave_6m_data, ','
   IF(tcnt==7) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_7m, ',', sep_ave_7m_data, ','
   IF(tcnt==8) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_8m, ',', sep_ave_8m_data, ','
   IF(tcnt==9) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_9m, ',', sep_ave_9m_data, ','
   IF(tcnt==10) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_10m, ',', sep_ave_10m_data, ','
   IF(tcnt==11) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_11m, ',', sep_ave_11m_data, ','
   IF(tcnt==12) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_12m, ',', sep_ave_12m_data, ','
   IF(tcnt>12) WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') ' ', ',', ' ' , ','
 !   WRITE(13,* ) ' separations in first year after hire'
   IF(tcnt==1) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_1m_posthire, ',', sep_ave_1m_posthire_data, ','
   IF(tcnt==2) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_2m_posthire, ',', sep_ave_2m_posthire_data, ','
   IF(tcnt==3) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_3m_posthire, ',', sep_ave_3m_posthire_data, ','
   IF(tcnt==4) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_4m_posthire, ',', sep_ave_4m_posthire_data, ','
   IF(tcnt==5) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_5m_posthire, ',', sep_ave_5m_posthire_data, ','
   IF(tcnt==6) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_6m_posthire, ',', sep_ave_6m_posthire_data, ','
   IF(tcnt==7) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_7m_posthire, ',', sep_ave_7m_posthire_data, ','
   IF(tcnt==8) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_8m_posthire, ',', sep_ave_8m_posthire_data, ','
   IF(tcnt==9) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_9m_posthire, ',', sep_ave_9m_posthire_data, ','
   IF(tcnt==10) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_10m_posthire, ',', sep_ave_10m_posthire_data, ','
   IF(tcnt==11) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_11m_posthire, ',', sep_ave_11m_posthire_data, ','
   IF(tcnt==12) WRITE(12,FMT='(2(F10.4,A1))', ADVANCE='NO') sep_ave_12m_posthire, ',', sep_ave_12m_posthire_data, ','
   IF(tcnt>12) WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') ' ', ',', ' ' , ','
WRITE(12,FMT='(A10,A1)') '-----', ','

END DO 

!! MONTHS 19 and 20: survival rates 
DO tcnt=19,20
    WRITE(12,FMT='(I10,A1)', ADVANCE='NO') tcnt, ','
!! CORRECTED MOBILITY
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdury_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdury_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdurp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'cmdurp_d', ','
!! RAW MOBILITY
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdury_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdury_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdurp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mdurp_d', ','
!! CYCLICAL MOB
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badcmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badcmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdcmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdcmdur_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdmdur_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdmdur_d', ','
    !! spells in distribution in good and bad times
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badnspel_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badnspel_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdnspel_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdnspel_d', ','
    !! conf interval
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badcm_lb_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'badcm_ub_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdcm_lb_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'gdcm_ub_d', ','
!! SURVIVAL
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_young_data(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix_prime_data(tcnt), ','
!! stayers survival
tempint=1!! adjustment
!tot_durationmatrix2_nocc_young_data
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_young_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_nocc_prime_data(tcnt-tempint), ','
!! movers survival
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_young(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_young_data(tcnt-tempint), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_prime(tcnt), ','
WRITE(12,FMT='(F10.4,A1)', ADVANCE='NO') tot_durationmatrix2_cocc_prime_data(tcnt-tempint), ','
!! HAZARDS
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'haz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'haz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'hazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'hazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'hazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'hazp_d', ','
!! stayers haz
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthaz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthaz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'sthazp_d', ','
!! movers haz
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhaz_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhaz_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhazy_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhazy_d', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhazp_m', ','
WRITE(12,FMT='(A10,A1)', ADVANCE='NO') ' ', ',' ! 'mvhazp_d', ','
!! separations after new hire
WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') ' ', ',' ! 'sepallyr_m', ',','sepallyr_d', ','
WRITE(12,FMT='(2(A10,A1))', ADVANCE='NO') ' ', ',' ! 'seprhire_m', ',','seprhire_d', ','
WRITE(12,FMT='(A10,A1)') '-----', ','

    
END DO 
CLOSE(12)

filename='dist_best'//trim(file_id)//trim(x1)//'.csv'
OPEN(UNIT=35, file=filename, form='formatted', status='replace')

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'p_index', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'x_index', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_index', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'p_level', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'x_level', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_level', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'prod', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'wage', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'pdist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'zdist', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_prime', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_young_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_prime_x', ','

!WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_x', ','
!WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_young_x', ','
!WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'epz_dist_prime_x', ','


WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist_prime', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist_young_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'upz_dist_prime_x', ','

! UNEMPLOYMENT DECOMPOSITION
    !REAL(8), DIMENSION(ppts) :: up_young_distribution_sim
    !REAL(8), DIMENSION(ppts) :: up_prime_distribution_sim
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'uallp_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_dist_prime', ','

WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_search_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_search_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_search_dist_prime', ','


WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_rest_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_rest_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_rest_dist_prime', ','


WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_reall_dist', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_reall_dist_young', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'up_reall_dist_prime', ','


!! distribution of (employed and unemployed over islands with different aggregate states
    !REAL(8), DIMENSION(ppts, zpts) :: upz_distribution_sim, epz_distribution_sim
    !REAL(8), DIMENSION(ppts, zpts) :: upz_young_distribution_sim, epz_young_distribution_sim
    !REAL(8), DIMENSION(ppts, zpts) :: upz_prime_distribution_sim, epz_prime_distribution_sim
    !! total rest, reallocation, search unemployment
    !! distribution of (employed and )unemployed with aggregate states



WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'e_dist_e1yb4_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'sep_dist_e1yb4_x', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_binary', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_binary', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_func', ','
WRITE(35, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_func', ','
WRITE(35, FMT='(A1)' ) ','



    !                                        epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
    !                                        epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim
    !

DO pcnt=1,ppts
DO xcnt=1, xpts
DO zcnt=1, zpts

WRITE(35, FMT='(I4, A1)' , ADVANCE='NO') pcnt, ','
WRITE(35, FMT='(I4, A1)' , ADVANCE='NO') xcnt, ','
WRITE(35, FMT='(I4, A1)' , ADVANCE='NO') zcnt, ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') pvector(pcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') xvector(xcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') zvector(zcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') pfunction(pcnt, xcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') wage(pcnt,xcnt,zcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') p_distribution_sim(pcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') znewpdf(zcnt), ','

! EMPLOYMENT PER AGE
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_distribution_sim(pcnt, zcnt), ','
!epz_x_young_distribution_sim, epz_x_prime_distribution_sim
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_young_distribution_sim(pcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_prime_distribution_sim(pcnt, zcnt), ','

! EMPLOYMENT PER HC LEVEL
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_x_distribution_sim(pcnt, xcnt, zcnt), ','
!epz_x_young_distribution_sim, epz_x_prime_distribution_sim
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_x_young_distribution_sim(pcnt, xcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_x_prime_distribution_sim(pcnt, xcnt, zcnt), ','



! UNEMPLOYMENT PER AGE
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_distribution_sim(pcnt, zcnt), ','
!epz_x_young_distribution_sim, epz_x_prime_distribution_sim
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_young_distribution_sim(pcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_prime_distribution_sim(pcnt, zcnt), ','

! UNEMPLOYMENT PER HC LEVEL
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_x_distribution_sim(pcnt, xcnt, zcnt), ','
!epz_x_young_distribution_sim, epz_x_prime_distribution_sim
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_x_young_distribution_sim(pcnt, xcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') upz_x_prime_distribution_sim(pcnt, xcnt, zcnt), ','


! UNEMPLOYMENT RATES, THEN SPLIT OVER SEARCH REST REALLOCATION
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') uallp_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_young_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_prime_distribution_sim(pcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_search_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_young_search_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_prime_search_distribution_sim(pcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_rest_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_young_rest_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_prime_rest_distribution_sim(pcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_reall_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_young_reall_distribution_sim(pcnt),  ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') up_prime_reall_distribution_sim(pcnt), ','

WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') epz_1yb4_x_distribution_sim(pcnt, xcnt, zcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') sep_epz_1yb4_x_distribution_sim(pcnt, xcnt, zcnt), ','

!! cutoffs
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliR(pcnt,xcnt, 1)==1) THEN
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliR(pcnt,xcnt, zcnt-1)==0 .AND. poliR(pcnt,xcnt, zcnt)==1) THEN
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! separation cutoff
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliDW(pcnt,xcnt, 1)==1) THEN
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliDW(pcnt,xcnt, zcnt-1)==0 .AND. poliDW(pcnt,xcnt, zcnt)==1) THEN
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        WRITE(35, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! cutoff z-values (as a function of p,x)
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') zr_res(pcnt, xcnt), ','
WRITE(35, FMT='(F12.6, A1)' , ADVANCE='NO') zs_res(pcnt, xcnt), ','
WRITE(35, FMT='(A1)' ) ','

END DO  !zcnt
END DO  !xcnt
END DO  ! pcnt

CLOSE(35)




!=============================================================
    !==============================================================
    !  CLOSING AN ITERATION
    !==============================================================
    !==============================================================

    ! update counter, and best distance
    estim_counter=estim_counter+1
    IF(mom1_dist<estim_best) estim_best=mom1_dist


    !!~~~~~~~~~~~~~~~~~~
    !! this is where the local allocatables come to pass
    !!~~~~~~~~~~~~~~~
    IF(verbose_progress) WRITE(*,*) 'deallocation'
    !$OMP CRITICAL
     DEALLOCATE(aggprod_ind)
      !$OMP END CRITICAL


      !$OMP CRITICAL
    DEALLOCATE(unempdur_estatus, data_u_qtr, data_uall_qtr, data_e_qtr, data_uadj18m_qtr, data_u_young_qtr,data_u_prime_qtr, data_uadj18m_young_qtr,data_uadj18m_prime_qtr, &
    data_e_young_qtr,data_e_prime_qtr, data_uraw_qtr, &
    data_u_qtr_p33, data_u_qtr_p50, data_u_qtr_p67, &
    data_ucompldur_occ_qtr, data_ucompldur_young_occ_qtr, data_ucompldur_prime_occ_qtr, &
    data_ucompldur_nocc_qtr, data_ucompldur_young_nocc_qtr, data_ucompldur_prime_nocc_qtr, &
    data_totduration_cocc_qtr, data_totduration_nocc_qtr, &
    data_totduration_cocc_young_qtr, data_totduration_nocc_young_qtr, &
    data_totduration_cocc_prime_qtr, data_totduration_nocc_prime_qtr, &
    data_compduration_cocc_qtr, data_compduration_nocc_qtr, &
    data_compduration_cocc_young_qtr, data_compduration_nocc_young_qtr, &
    data_compduration_cocc_prime_qtr, data_compduration_nocc_prime_qtr, &
    data_totduration_cr_cocc_qtr, data_totduration_cr_nocc_qtr, &
    data_totduration_cr_cocc_young_qtr, data_totduration_cr_nocc_young_qtr, &
    data_totduration_cr_cocc_prime_qtr, data_totduration_cr_nocc_prime_qtr, &
    data_v_qtr,data_theta_qtr, &
    data_jf_qtr, data_jf_young_qtr, data_jf_prime_qtr, &
    data_prod_qtr, data_wage_qtr, data_sep_qtr, data_sep_y_qtr, data_sep_p_qtr, &
    data_cocc_inflow_qtr, data_cocc_young_inflow_qtr, data_cocc_prime_inflow_qtr, data_nocc_inflow_qtr, &
    data_nocc_young_inflow_qtr, data_nocc_prime_inflow_qtr, &
    data_cocc_outflow_qtr, data_cocc_young_outflow_qtr, data_cocc_prime_outflow_qtr, data_nocc_outflow_qtr, &
    data_nocc_young_outflow_qtr, data_nocc_prime_outflow_qtr, &
    data_cocc_qtr, data_cocc_young_qtr, data_cocc_prime_qtr, data_nocc_qtr, data_nocc_young_qtr, data_nocc_prime_qtr, &
    data_pocc_qtr, data_pnocc_qtr, data_pocc_young_qtr, data_pnocc_young_qtr, data_pocc_prime_qtr, data_pnocc_prime_qtr, data_young_qtr, data_prime_qtr, &
    data_ulev_qtr, &
    data_lsep_qtr, data_lsep_y_qtr, data_lsep_p_qtr, data_ljf_qtr, &
    data_ljf_young_qtr, data_ljf_prime_qtr, data_lpocc_qtr, data_lpnocc_qtr, &
    data_ult5wk_qtr, data_u5lt15wk_qtr, data_u15lt27wk_qtr, data_ugt27wk_qtr, &
    data_l_ult5wk_qtr, data_l_u5lt15wk_qtr, data_l_u15lt27wk_qtr, data_l_ugt27wk_qtr, &
    data_ljf5_qtr, data_ljf5_young_qtr, data_ljf5_prime_qtr, data_lpocc5_qtr, data_lpnocc5_qtr, &
    data_scratch_qtr, data_lv5_qtr, data_lu5_qtr, data_luall5_qtr, data_ltheta5_qtr, &
    data_lsep5_qtr, data_lprod5_qtr, data_lwage5_qtr, &
    data_lcocc_outflow5_qtr, data_lcocc_young_outflow5_qtr, data_lcocc_prime_outflow5_qtr, &
    data_u_yr, data_sep_y_yr, data_sep_p_yr, data_jf_y_yr, data_jf_p_yr, data_u_y_yr, data_u_p_yr, &
    data_sm5_ult3m_qtr, data_sm5_ult5m_qtr, &
    data_sm5_ult9m_qtr, data_sm5_ult13m_qtr, data_sm5_ugt13m_qtr, &
    data_ult3m_qtr , data_ult5m_qtr , &
    data_ult9m_qtr , data_ult13m_qtr , data_ugt13m_qtr , &
    data_l_ult3m_qtr , data_l_ult5m_qtr , &
    data_l_ult9m_qtr , data_l_ult13m_qtr , data_l_ugt13m_qtr , &
    data_ult3m_yng_qtr , data_ult5m_yng_qtr , &
    data_ult9m_yng_qtr , data_ult13m_yng_qtr , data_ugt13m_yng_qtr , &
    data_l_ult3m_yng_qtr , data_l_ult5m_yng_qtr , &
    data_l_ult9m_yng_qtr , data_l_ult13m_yng_qtr , data_l_ugt13m_yng_qtr , &
    data_ult3m_prm_qtr , data_ult5m_prm_qtr , &
    data_ult9m_prm_qtr , data_ult13m_prm_qtr , data_ugt13m_prm_qtr , &
    data_l_ult3m_prm_qtr , data_l_ult5m_prm_qtr , &
    data_l_ult9m_prm_qtr , data_l_ult13m_prm_qtr , data_l_ugt13m_prm_qtr &
    )
      !$OMP END CRITICAL
    IF(verbose_progress) WRITE(*,*) 'finished deallocation'
    !!~~~~~~~ note that we can deal with

    202 FORMAT (2(I14, A1), F14.4, A1, I14, A1, F14.4, A1, 5(I14, A1))

    ! further deallocate memory
    !$OMP CRITICAL
    DEALLOCATE(poliDW, & !VE, VU, EV, & !TH, TH_U, DmaxU,Dmax,
                        JF, poliR, WAGE)
    !$OMP END CRITICAL


        ! report duration of program
    CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
    call cpu_time ( t2 )
    !WRITE(10,*) 'date=', time_string(1), ',end:', time_string(2), ',start:', time_string_begin(2), ',CPUtime:', t2-t1, ',dist:', mom1_dist, 'thread', threadnumber
    !WRITE(*,*) 'date=', time_string(1), ',end:', time_string(2), ',start:', time_string_begin(2), ',CPUtime:', t2-t1, ',dist:', mom1_dist, 'thread', threadnumber
    !WRITE(*,*) 'date=', time_string(1), 'end: ', time_string(2), 'start:', time_string_begin(2), 'CPU time:', t2-t1, 'dist'

    ! stop if it is a one-off trial run
    IF(onerun_pauze==1) THEN
        WRITE(*,*) 'one run completed: PAUSED'
        PAUSE
    END IF

    ! close reporting file
    WRITE(10,*) 'date=', time_string(1), ',end:', time_string(2), ',start:', time_string_begin(2), ',CPUtime:', t2-t1, ',dist:', mom1_dist, 'thread', threadnumber
    CLOSE(10)

    !=========================
    ! BEEEEEEEEEEP
    !=====================


    ! beep to signal run end
    !Print *, char(7)


            !===========================================
            ! WRITE TO MOM VECTOR (V_ERR)
            !===========================================

    DO i=1, nmom_local !nmom_local
        IF((omega(i,i) .ne. omega(i,i)+1.0_8) .AND. omega(i,i) .ge. 0.0_8) THEN
             mom(i)=sqrt(omega(i,i))
        ELSE IF (omega(i,i) == omega(i,i)+1.0_8) THEN
             mom(i)=omega(i,i)
        ELSE IF (omega(i,i)<0.0_8) THEN
             mom(i)=HUGE(1.0_8)
        END IF
     END DO


 !==================================================================================================================================================


    !****************************************************************************************
    ! ADDITIONAL PARTS/FUNCTIONS
    !****************************************************************************************


    CONTAINS

     REAL(8) FUNCTION pfunction(pindex, xindex, zindex)
            INTEGER(4), INTENT(IN) :: pindex, xindex, zindex

                pfunction=zvector(zindex)*pvector(pindex)*xvector(xindex)

     END FUNCTION pfunction


     REAL(8) FUNCTION pfunction_pz(pindex, zindex)
            INTEGER(4), INTENT(IN) :: pindex, zindex

                pfunction_pz=zvector(zindex)*pvector(pindex)

     END FUNCTION pfunction_pz

! variance calculation
REAL(8) FUNCTION VARCALCUL(vector, tmax_qtr_local)

    REAL(8), DIMENSION(:), INTENT(IN) :: vector
    INTEGER, INTENT(IN) :: tmax_qtr_local
    INTEGER :: vectorsize, tempint,t
    REAL(8) :: tempvar, tempaverage

    vectorsize=SIZE(vector, dim=1)
    IF (vectorsize<tmax_qtr_local-2) WRITE(*,*) 'ERROR --- VECTOR SIZE IN VARIANCE CALCULATION RUBBISH: check variance function and its inputs'

    tempvar=0.0_8
    tempaverage=0.0_8
    tempint=0
    DO t=1, MAX(tmax_qtr_local-2,1)
     tempvar=tempvar+vector(t)**2.0_8
     tempaverage=tempaverage+vector(t)
     tempint=tempint+1
    END DO
    tempaverage=tempaverage/REAL(tempint)
    varcalcul=(tempvar/REAL(tempint))-(tempaverage)**2.0_8
END FUNCTION varcalcul


! corr calculation
REAL(8) FUNCTION CORRCALCUL(vector1, vector2, tmax_qtr_local)

    REAL(8), DIMENSION(:), INTENT(IN) :: vector1, vector2
    INTEGER, INTENT(IN) :: tmax_qtr_local
    INTEGER :: vectorsize, tempint,t
    REAL(8) :: tempcorr, tempaverage1, tempaverage2, tempvar1, tempvar2

    vectorsize=MIN(SIZE(vector1, dim=1), SIZE(vector2, dim=1))
    IF (vectorsize<tmax_qtr_local-2) WRITE(*,*) 'ERROR --- VECTOR SIZE IN CORRELATION CALCULATION RUBBISH: check correlation function and its inputs'

    tempcorr=0.0_8
    tempaverage1=0.0_8
    tempaverage2=0.0_8
    tempvar1=0.0_8
    tempvar2=0.0_8
    tempint=0
    DO t=1, MAX(tmax_qtr_local-2,1)
     tempcorr=tempcorr+(vector1(t)*vector2(t))
     tempvar1=tempvar1+vector1(t)**2.0_8
     tempvar2=tempvar2+vector2(t)**2.0_8
     tempaverage1=tempaverage1+vector1(t)
     tempaverage2=tempaverage2+vector2(t)
     tempint=tempint+1
    END DO
    tempaverage1=tempaverage1/REAL(tempint)
    tempaverage2=tempaverage2/REAL(tempint)
    tempvar1=(tempvar1/REAL(tempint))-(tempaverage1)**2.0_8
    tempvar2=(tempvar2/REAL(tempint))-(tempaverage2)**2.0_8
    tempcorr=(tempcorr/REAL(tempint))-(tempaverage1)*(tempaverage2)
    corrcalcul=tempcorr/(SQRT(tempvar1)*SQRT(tempvar2))
END FUNCTION CORRCALCUL


! prescott's HP Filter code
!C ----------------------------------------------------------------------
!C  SR: hpfilt
!C  Kalman smoothing routine for HP filter written by E Prescott.
!C   y=data series, d=deviations from trend, t=trend, n=no. obs,
!C   s=smoothing parameter (eg, 1600 for std HP).
!C   Array v is scratch area and must have dimension at least 3n.
!C   If IOPT=1 and n and s are the same as for the previous call,
!C   the numbers in v are not recomputed.  This reduces execution
!C   time by about 30 percent.  Note that if this option is exercised,
!C   v cannot be used for other purposes between calls.
!C   This version does NOT release the trend in order to save memory.
!C ----------------------------------------------------------------------

 SUBROUTINE HPFILT(Y,D,V,N,S,IOPT)

      INTEGER(4) :: IOPT,NN,I,I1,IB,N
      REAL(8), DIMENSION(:), INTENT(IN) :: Y
      REAL(8), DIMENSION(N) :: T(N)
      REAL(8), DIMENSION(:,:), INTENT(INOUT) :: V
      REAL(8), INTENT(OUT) :: D(N)

      REAL(8), INTENT(IN) :: S
      REAL(8) :: SS

      REAL(8) :: M1, M2, V11, V12, V22, X, Z, B11, B12, B22, DET, E1, E2


      DATA SS, NN/0.D0,0/



!      REAL(8) :: Y(144),T(144),V(144,3),D(144),SS
!      REAL(8) :: M1, M2, V11, V12, V22, X, Z, B11, B12, B22, DET, E1, E2, S
!      INTEGER(4) IOPT,NN,I,I1,IB,N
!      DATA SS, NN/0.D0,0/

!C
!C     compute sequences of covariance matrix for f[x(t),x(t-1) | y(<t)]
!C
      IF(IOPT .NE. 1 .OR. NN .NE. N  .OR. S .NE. SS)  THEN
        SS=S
        NN=N
        V11=1.0_8
        V22=1.0_8
        V12=0.0_8
       DO 5 I=3,N
        X=V11
        Z=V12
        V11=1.D0/S + 4.D0*(X-Z) + V22
        V12=2.D0*X - Z
        V22=X
        DET=V11*V22-V12*V12
        V(I,1)=V22/DET
        V(I,3)=V11/DET
        V(I,2)=-V12/DET
        X=V11+1.D0
        Z=V11
        V11=V11-V11*V11/X
        V22=V22-V12*V12/X
        V12=V12-Z*V12/X
  5    CONTINUE
                                       ENDIF
!C
!C     this is the forward pass
!C
      M1=Y(2)
      M2=Y(1)
      DO 10 I=3,N
        X=M1
        M1=2.0*M1-M2
        M2=X
        T(I-1)= V(I,1)*M1+V(I,2)*M2
        D(I-1)= V(I,2)*M1+V(I,3)*M2
        DET=V(I,1)*V(I,3)-V(I,2)*V(I,2)
        V11=V(I,3)/DET
        V12=-V(I,2)/DET
        Z=(Y(I)-M1)/(V11+1.D0)
        M1=M1+V11*Z
        M2=M2+V12*Z
 10     CONTINUE
      T(N)=M1
      T(N-1)=M2
!C
!C       this is the backward pass
!C
         M1=Y(N-1)
         M2=Y(N)
        DO 15 I=N-2,1,-1
           I1=I+1
           IB=N-I+1
         X=M1
         M1=2.D0*M1 - M2
         M2=X
!C
!C           combine info for y(.lt.i) with info for y(.ge.i)
!C
         IF(I.GT.2)                 THEN
           E1=V(IB,3)*M2 + V(IB,2)*M1 + T(I)
           E2=V(IB,2)*M2 + V(IB,1)*M1 + D(I)
           B11=V(IB,3)+V(I1,1)
           B12=V(IB,2)+V(I1,2)
           B22=V(IB,1)+V(I1,3)
           DET=B11*B22-B12*B12
           T(I)=(-B12*E1+B11*E2)/DET
                                    ENDIF
!C
!C           end of combining
!C
        DET=V(IB,1)*V(IB,3)-V(IB,2)*V(IB,2)
        V11=V(IB,3)/DET
        V12=-V(IB,2)/DET
        Z=(Y(I)-M1)/(V11+1.D0)
         M1=M1+V11*Z
         M2=M2+V12*Z
 15     CONTINUE
       T(1)=M1
       T(2)=M2
        DO 20 I=1,N
 20      D(I)=Y(I)-T(I)
        RETURN


END SUBROUTINE HPFILT


recursive function quantile( k, a) result( value)
    integer,             intent (in)    :: k          ! position in array
    real(8), dimension (:), intent (in) :: a          ! array in
    real(8)                             :: value      ! output value of quantile
    integer                             :: j
    real(8)                             :: ak
    ak = a( k)
    j = count( a < ak)                                  ! how many a(:) < ak
    if( j >= k) then
        value = quantile( k, pack( a, a < ak))
    else
	    j = count( a > ak) + k - size( a)
	    if( j > 0) then
		    value = quantile( j, pack( a, a > ak))
	    else
		    value = ak
        end if
    end if

end function quantile


recursive function wquantile(targetpctile, a, ww) result( value)
    real(8),             intent (in)    :: targetpctile          ! percentile (fractional notation)
    !real(8), intent(in)                 :: vecsize
    real(8), dimension (:), intent (in) :: a, ww          ! array in
    real(8), dimension(size(ww))        :: wwl
    REAL(8)                             :: targetpctilel
    real(8)                             :: value      ! output value of quantile
    integer                             :: counter1,j,indx,k, vecsize
    real(8)                             :: temppctile
    real(8)                             :: ak

    ! establish the index of the vector where adding up all mass up to and including the element, mass equals or for the first time EXCEEDS the percentile cutoff
    ! i.e. target percentile 0, first element exceeds it. (we count from the 0th percentile to the 99th percentile, perhaps misnaming things a bit!)
    targetpctilel=targetpctile
    ! Normalize weights to 1.0: this means that we have to adjust the targetpercentile we feed in recursively, because the remaining distribution is 'censored'
    IF(sum(ww(:))>0.0_8) THEN

    wwl=ww/sum(ww(:))

    ! find the index at which total probaiblity passes the targetpctile cutoff
    k=1
    temppctile=0.0_8
    establish_count_do: DO counter1=1,size(a)
        temppctile=temppctile+wwl(counter1)
        IF(temppctile>=targetpctilel .OR. k==size(a)) EXIT establish_count_do
        k=k+1
    END DO establish_count_do

    ! the value attached to said index
    ak = a( k)
    j=count( a< ak)                              ! how many a(:) < ak: size of vector considered subsequently

    ! total mass of those with values strictly less
    temppctile=0.0_8
    DO counter1=1,size(a)
        IF(a(counter1)<ak) temppctile=temppctile+wwl(counter1)
    END DO

    !! if more mass is contained than the target percentile, below ak, we repeat the exercise, but only consider those values strictly below ak
    if( temppctile >=targetpctilel .AND. j>0 .AND. temppctile>0.0_8) then
        !adjust target percentile
        IF(temppctile>0.0_8) targetpctilel=targetpctilel/temppctile
        value = wquantile(targetpctilel, pack( a, a < ak), pack(wwl, a<ak))
    elseif (temppctile >=targetpctilel .AND. j==0) then
        value=ak
    elseif (temppctile==0.0_8 .and. targetpctilel ==0.0_8) then
        value=ak
    else
    !! if less mass contained strictly below ak, then two possibilities: either we need to add strictly positive mass that is strictly above ak, or
    !! adding the mass at ak will make the mass counter jump over the percentile target, in which case we have found the value at the targetpctilel percentile
	    !j = count( a > ak) + k - size( a)          ! j>0 implies size(a)-k< count(a>ak)
	    !if( j > 0) then
		!    value = quantile( j, pack( a, a > ak))
	    !else
		!    value = ak
        !end if

        !targetpctilel= mass(a>ak) + targetpctilel-1.0_8               ! mass strictly below ak is strictly lower than target. New target is to look for 1.0-
                                                                    ! temppctile+wwl(k)=mass(a>ak)
                                                                    ! targetpctilel=targetpctilel-mass<=ak
                                                                    ! which equals targetpctilel=targetpctilel-temppctile-wwl(k)
        targetpctilel=targetpctilel-temppctile-wwl(k)
        if (targetpctilel>0.0_8) then
            targetpctilel=targetpctilel/(1.0_8-temppctile-wwl(k))               ! adjust targetpercentile
            value= wquantile(targetpctilel, pack(a, a>ak), pack(wwl,a>ak))
        else
            value=ak
        end if

    end if
else if (SUM(REAL(ww(:)))==0 .AND. size(a)>0) then
    value=-1000000000
else
    value=-HUGE(1.0_8)
end if

end function wquantile

SUBROUTINE UNBIASED_CORRELATION( N, A, B, msg, lag, r, t, m)

  ! Computes the correlation of two series A and B of length
  ! N as a function of lag. The correlation is unbiased
  ! because the sums in the covariance and standard devia-
  ! tions are divided by m, not m-1, where m is the number
  ! of overlapping grid points.

  ! The correlation r is defined as
  !
  !           cov(A,B)
  !     r = -------------
  !          s(A) * s(B)
  !
  ! where cov() is covariance and s() is standard deviation.

  ! The series A and B must be prepared such that they
  ! are sent to this routine with zero lag. If necessary, this
  ! is accomplished by padding the beginning or ends (or both)
  ! of each time series with missing values (msg) so that their
  ! elements correspond one to one. The resultant time series
  ! will be of equal length N. Also, missing values (msg)
  ! distributed thoughout each time series is perfectly
  ! acceptable. Time series B will be shifted by the amount
  ! lag relative to time series A:

  ! RETURNS: r, the unbiased correlation, t, the significance
  ! of the unbiased correlation ( t is set to msg if B = A
  ! at lag 0, i.e. an autocorrelation at lag 0, or if the
  ! correlation is 1.0 in general), and m, the number of
  ! overlapping grid points.

  ! A:      t1  t2  t3  t4        ...       tN
  ! B:              t1  t2  t3  t4        ...       tN

  !         \_ _/
  !           V
  !           lag = 2 (Defined to be > 0.)

  IMPLICIT NONE

  INTEGER, INTENT(IN)                :: N
  REAL(8), DIMENSION(N), INTENT(IN)  :: A
  REAL(8), DIMENSION(N), INTENT(IN)  :: B
  REAL(8), INTENT(IN)                :: msg
  INTEGER, INTENT(IN)                :: lag
  REAL(8), INTENT(OUT)               :: r
  REAL(8), INTENT(OUT)               :: t
  INTEGER, INTENT(OUT)               :: m


  REAL, DIMENSION(:), ALLOCATABLE :: x
  REAL, DIMENSION(:), ALLOCATABLE :: y
  REAL, DIMENSION(:), ALLOCATABLE :: xdev
  REAL, DIMENSION(:), ALLOCATABLE :: ydev
  REAL, DIMENSION(:), ALLOCATABLE :: xdevydev
  REAL, DIMENSION(:), ALLOCATABLE :: xdevxdev
  REAL, DIMENSION(:), ALLOCATABLE :: ydevydev

  REAL                            :: xmn
  REAL                            :: ymn
  REAL                            :: COVxy
  REAL                            :: Sx
  REAL                            :: Sy


  ALLOCATE( x(1:3*N) )
  ALLOCATE( y(1:3*N) )

  x(:) = msg
  y(:) = msg

  x(N+1:2*N) = A(:)
  y(N+1:2*N) = B(:)

  y = EOSHIFT( y, SHIFT = -lag, BOUNDARY = msg )

  WHERE ( x == msg ) y = msg
  WHERE ( y == msg ) x = msg

  m = COUNT( x /= msg )

  IF ( m < 3 ) THEN

   WRITE (*,'(A40, I1)') "The number of overlapping points is m = ", m
   WRITE (*,'(A35)')     "The value of m should be 3 or more."
   WRITE (*,'(A17)')     "Decrease the lag."
   WRITE (*,'(A52)')     "Execution halted in SUBROUTINE UNBIASED_CORRELATION."
   STOP

  END IF

  xmn = SUM( x, DIM = 1, MASK = x /= msg ) / REAL(m)
  ymn = SUM( y, DIM = 1, MASK = y /= msg ) / REAL(m)

  ALLOCATE( xdev(1:3*N) )
  ALLOCATE( ydev(1:3*N) )

  xdev(:) = x(:) - xmn
  WHERE ( x == msg ) xdev(:) = msg

  ydev(:) = y(:) - ymn
  WHERE ( y == msg ) ydev(:) = msg

  ALLOCATE( xdevydev(1:3*N) )

  xdevydev(:) = xdev(:) * ydev(:)
  WHERE ( x == msg ) xdevydev(:) = msg

  COVxy = SUM( xdevydev, DIM = 1, MASK = xdevydev /= msg ) / REAL(m)

  ALLOCATE( xdevxdev(1:3*N) )

  xdevxdev(:) = xdev(:) * xdev(:)
  WHERE ( x == msg ) xdevxdev(:) = msg

  Sx = SQRT( SUM( xdevxdev, DIM = 1, MASK = xdevxdev /= msg ) / REAL(m) )

  ALLOCATE( ydevydev(1:3*N) )

  ydevydev(:) = ydev(:) * ydev(:)
  WHERE ( y == msg ) ydevydev(:) = msg

  Sy = SQRT( SUM( ydevydev, DIM = 1, MASK = ydevydev /= msg ) / REAL(m) )

  r = COVxy / ( Sx * Sy )

  IF ( r /= 1.0 ) THEN

    t = r * SQRT( (m - 2) / ( 1 - r*r ) )

  ELSE

    t = msg

  END IF

  DEALLOCATE( x, y, xdev, ydev, xdevydev, xdevxdev, ydevydev )

  END SUBROUTINE UNBIASED_CORRELATION

subroutine bkfilter(n, y, up, dn, k, ybp)

! Aubhik 20.02.2005
!
! Baxter and King (1999) Band-Pass filter
!
! y is a data vector of length n
! ybp is the band pass filtered series, with 0.0 for the first and last k observations
! n is the length of the data vector y
! up is the lower frequency
!	for example, the business cycle band will have up = 6 for quarterly data
!	and up = 1.5 for annual data
! dn is the higher frequency
!	for example, the business cycle band will have dn = 32 for quarterly data
!	and dn = 8 for annual data
! k is the number of leads and lags used by the symmetric moving-average
!	representation of the band pass filter.  Baxter and King (1999) find
!	that k = 12 is a practical choice.
!
! Algorithm provided by Bob King.  This subroutine is the Fortran 90 version of
! bpf.m and filtk.m written by Baxter and King (1999).  The band pass filter is
! a moving average involving 2k+1 terms.  The weights are symmetric and calculated
! in akvec.

implicit none

integer, parameter:: rk = selected_real_kind(15,307), ik = selected_int_kind(9)

integer(ik):: n, up, dn, k
real(rk):: y(n), ybp(n)

integer(ik):: j
real(rk):: pi, omlbar, omubar, kr, kj, theta
real(rk):: akvec(k+1), avec(2*k+1)

intent(in):: n, up, dn, k, y
intent(out):: ybp

pi = 2.0_rk*acos(0.0_rk)

omlbar = 2.0_rk*pi/dble(dn)
omubar = 2.0_rk*pi/dble(up)

akvec = 0.0_rk
avec = 0.0_rk

kr = dble(k)

akvec(1) = (omubar - omlbar)/pi

do j = 1, k
    kj = dble(j)
    akvec(j+1) = (dsin(kj*omubar) - dsin(kj*omlbar))/(kj*pi)
end do

theta = akvec(1) + 2.0_rk*sum(akvec(2:k+1))
theta = -1.0_rk*(theta/(2.0_rk*kr + 1.0_rk))

akvec = akvec + theta

avec(k+1) = akvec(1)

do j = 1, k
    avec(k+1 - j) = akvec(j+1)
    avec(k+1 + j) = akvec(j+1)
end do

ybp = 0.0_rk

do j = k + 1, n - k
    ybp(j) = dot_product(avec, y(j - k: j + k))
end do

end subroutine bkfilter

    END SUBROUTINE CTV_function_simple




END MODULE mod_solve_ctv
