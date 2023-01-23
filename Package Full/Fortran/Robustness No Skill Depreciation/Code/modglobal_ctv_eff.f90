
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

    
    
!
!    THIS VERSION INCORPORATES OCCUPATIONAL HUMAN CAPITAL
!
!
!***********************************************************************
!
!             GLOBAL DECLARATIONS OF --- COMPUTATION PARAMETERS (not CTV model parameter values)
!                                                                --- simulation parameters
!                                                                --- other computational parameters
!                                                                --- production and matching function
!************************************************************************






MODULE modglobal_ctv_eff

USE ctv_grid_mod

IMPLICIT NONE


!---------------
! PARALLELISM GLOBALS
!------------------

!INTEGER, PARAMETER :: omp_maxthreads=16


!----------------------------------------
! CALIBRATED PARAMETERS (Monthly/Quarterly model)
!----------------------------------------

!SET - utility
    REAL(8) :: bt=0.9997_8                   !discount factor (BE CAREFUL: total discounting involves bt AND dd, the latter the prob. of end of working life
    REAL(8), PARAMETER :: rr=(1.04_8)**(1.0_8/12.0_8)   !gross real interest rate (based on 4% annual)
    REAL(8), PARAMETER :: mc=1.0_8
    REAL(8), PARAMETER :: miscoding_par=0.095_8 !0.089_8
    REAL(8), PARAMETER :: b_set=0.75_8

    INTEGER :: instantreall=0
    CHARACTER*12 :: file_id='noskdep_'
    INTEGER :: prodfunc_add
    REAL(8) :: hpfilterfactor=1600.0_8

    REAL(8) :: dd=0.00048_8                      ! probability of end of working life
    REAL(8) :: miscodingbar

    REAL(8) :: delta, deltalowhc, deltahighhc          ! exogenous match breakup rate, matched in SMM
    REAL(8) :: k                             ! vacancy cost
    REAL(8) :: reallc                      ! reallocation cost
    REAL(8) :: eta                           ! elasticity of the matching function: = (1-BARGAINING POWER OF WORKERS)  !!!!!
    REAL(8) :: rho_z                      ! autoregressive coefficient z
    REAL(8) :: rho_p                      ! autoregressive coefficient p
    REAL(8) :: sigma_z                  ! st dev error z innovation
    REAL(8) :: sigma_p                  ! st dev erro p innovation
    REAL(8) :: b                            ! value of unemployment
    REAL(8) :: sigma_x                  ! standard deviation of match-specific productivity: this will developed later?
    REAL(8) :: directed_parameter              !thetal(13)           ! top proportion where reallocators search
    REAL(8) :: skilldep


    INTEGER :: matrix_statistics=0
    !INTEGER :: nsim

    REAL(8), PARAMETER :: epsie=1.0e-3_8    !small amount

!SMM - preferences; matching; firm
    !REAL(8), PARAMETER :: sigma=2.0_8       !intertemporal elasticity of substitution
    !REAL(8), PARAMETER :: eta=0.7_8         !matching elasticity in Cobb-Douglas
    !REAL(8), PARAMETER :: lamb_e=0.7_8      !offer arrival OTJ
    !REAL(8), PARAMETER :: k=2.5_8           !vacancy posting cost
    !REAL(8):: lweibull                      ! SCALE parameter of the WEIBULL distribution: lower is more spread-out
    !REAL(8):: kweibull                      ! SHAPE parameter of the weibull distribution: lower moves mass to the lower zs
    !REAL(8) :: zmin                         ! SHIFT parameter for the minimum of the weibull distribution




!------------------------------------------
! moments: dimension
!--------------------------------------------

INTEGER, PARAMETER :: npar=14
INTEGER, PARAMETER :: nmom=74          !=12           !Number of moments used as targets


!-----------------------------
! ADDED MODEL ELEMENTS
!-----------------------------

    INTEGER :: restricted_search_ind
    REAL(8), PARAMETER :: occmob_correction=0.0_8
    REAL(8) :: miscoding_estimation
    INTEGER, PARAMETER :: miscoding_estimation_ind=0
    INTEGER, PARAMETER :: miscoding_parameter_ind=1



!--------------------------
! PRODUCTIVITY GRID
!---------------------------
        ! gridpoints themselves set in grid_global_ctv

!---------------------------
!   OCCUPATION-WORKER PRODUCTIVITY

!INTEGER(2), PARAMETER :: zpts=80     !50        ! basic grid (later made smaller, if without loss of results)
REAL(8), PARAMETER :: zlambda=2.5_8        ! 2.5_8          ! number of standard deviations filled in, in the tauchen procedure
INTEGER, PARAMETER :: zprecision=0           ! extra fine grid at the bottom of the zdistribution
INTEGER, PARAMETER :: restricted_search_parameter=1 !

!---------------------------
!   HUMAN CAPITAL PRODUCTIVITY

INTEGER(1), PARAMETER :: islandhet_only=1
!INTEGER, PARAMETER :: xpts=3
!REAL(8), PARAMETER :: xwidth=0.01_8
REAL(8) :: xmin
REAL(8) :: xmax
REAL(8), DIMENSION(xpts) :: xvector_gl
REAL(8), DIMENSION(xpts) :: xpdfvector_gl, xcdfvector_gl

!---------------------------
!   AGGREGATE PRODUCTIVITY

!INTEGER(2), PARAMETER :: ppts=15   ! basic grid for aggregate shocks
REAL(8), PARAMETER  :: plambda=3.0 !2.5 !2.5 !3.0 !3.0 !2.0 !3.0 !2.0 !3.0 !3.0 STANDARD IN 2019-2020 !2.5_8
INTEGER, PARAMETER :: pprecision=0           ! no extra fine grid at the bottom


!!--------------------------
!! PRODUCTIVITY GRID
!!---------------------------
!
!!---------------------------
!!   OCCUPATION-WORKER PRODUCTIVITY
!
!INTEGER(2), PARAMETER :: zpts=80     !50        ! basic grid (later made smaller, if without loss of results)
!REAL(8), PARAMETER :: zlambda=2.0_8                 ! number of standard deviations filled in, in the tauchen procedure
!INTEGER, PARAMETER :: zprecision=0           ! extra fine grid at the bottom of the zdistribution
!INTEGER, PARAMETER :: restricted_search_parameter=1 !
!
!!---------------------------
!!   HUMAN CAPITAL PRODUCTIVITY
!
!INTEGER(1), PARAMETER :: islandhet_only=1
!INTEGER, PARAMETER :: xpts=3
!!REAL(8), PARAMETER :: xwidth=0.01_8
!REAL(8) :: xmin
!REAL(8) :: xmax
!REAL(8), DIMENSION(xpts) :: xvector_gl
!REAL(8), DIMENSION(xpts) :: xpdfvector_gl, xcdfvector_gl
!
!!---------------------------
!!   AGGREGATE PRODUCTIVITY
!
!INTEGER(2), PARAMETER :: ppts=15   ! basic grid for aggregate shocks
!REAL(8), PARAMETER  :: plambda=1.96_8
!INTEGER, PARAMETER :: pprecision=0           ! no extra fine grid at the bottom
!
!!----------------------------
!!  VE, VU
!!-----------------------------
!
!!REAL(8), DIMENSION(ppts, xpts, zpts) :: VE=0.0_8
!!REAL(8), DIMENSION(ppts, xpts, zpts) :: VU=0.0_8
!--------------------------
! BACKWARDS INDUCTION GLOBALS
!-------------------------

    REAL(8), PARAMETER :: convcrit=0.00001_8
    INTEGER(4), PARAMETER :: tmax=2  ! just today and tomorrow in the fixed point
    INTEGER(2), PARAMETER :: maxrunnumber=6000 !100 !3700      !2500       ! 4000

!-----------------------------------
! SIMULATION GLOBALS
!-----------------------------------

    !! OLDER VERSION
    INTEGER(2), PARAMETER :: resolution=1000
    INTEGER, PARAMETER :: max_gen=1                 ! number of generations in simulation
    INTEGER, PARAMETER :: nsim=100000
    INTEGER, PARAMETER :: onedrawrandomness=0               ! draw all random numbers for a generation and time at once
    !REAL  ::  bigworkrandom1, bigworkrandom2
    INTEGER, PARAMETER :: tmax_sim=20000 !40000  !20000
    INTEGER, PARAMETER :: tmax_sim2=20000 !40000 !20000
    INTEGER, PARAMETER :: tmin_sim2basic=6000               ! should be nontrivial due to overlapping gen; for program to work at all has to be bigger than 12
    INTEGER :: tmin_sim2
    INTEGER, PARAMETER :: nsim_gen=10000
    INTEGER, PARAMETER :: sample_timetogo_int=16
    INTEGER, PARAMETER :: tmin_cutoff=1000

    
    !! NEWER VERSION, NOT USED FOR (OLDER) NO SKILL DEPRECIATION VERSION
    !INTEGER(2), PARAMETER :: resolution=1000
    !INTEGER, PARAMETER :: max_gen=1                 ! number of generations in simulation
    !INTEGER, PARAMETER :: nsim=20000
    !INTEGER, PARAMETER :: onedrawrandomness=0               ! draw all random numbers for a generation and time at once
    !!REAL  ::  bigworkrandom1, bigworkrandom2
    !    ! no reallocation: tmax_sim = 20000?
    !    ! excessmob: tmax_sim=40000? 
    !INTEGER, PARAMETER :: tmax_sim=20000 !!40000 !20000 !100000! 100000!334000 !20000!90060 !20000 !20000 is number used in 15v4
    !INTEGER, PARAMETER :: tmax_sim2=20000 !!40000 !20000 !100000   !334000 !20000!90060 !20000 !20000 is number used in 15v4
    !INTEGER, PARAMETER :: tmin_sim2basic=6000               ! should be nontrivial due to overlapping gen; for program to work at all has to be bigger than 12
    !INTEGER :: tmin_sim2
    !INTEGER, PARAMETER :: nsim_gen=20000
    !INTEGER, PARAMETER :: sample_timetogo_int=0
    !INTEGER, PARAMETER :: tmin_cutoff=1000

    INTEGER, PARAMETER :: tmax_swindow_wks_raw=1680             ! 35 years
    
    INTEGER, PARAMETER :: moverstayer_regr_lindetrending=1
!-----------------------------------
!  RUN OPTIONS AND OUTPUT OPTIONS
!-----------------------------------

    ! NOTE THAT nsim_gen are in modglobal
    ! NOTE THAT tmax_sim2 are in modglobal
    INTEGER(1), PARAMETER :: onerun_pauze=0
    INTEGER(1), PARAMETER :: verboseswitch=0
    INTEGER(1), PARAMETER :: verbose_backw_ind=0
    INTEGER(1), PARAMETER :: verboseswitch_l2=0
    INTEGER(1), PARAMETER :: verboseswitchfile=0
    INTEGER(1), PARAMETER :: verbosesimulation=0
    INTEGER(1), PARAMETER :: verbosetimeseriesfile=0
    INTEGER(1), PARAMETER :: workerhistory_sim=0

    INTEGER(1), PARAMETER :: verbose_cutoffs=0
    INTEGER(1), PARAMETER :: verbose_cutoffs2=0
    INTEGER(1), PARAMETER :: verbose_results_l0=0           ! business cycle matrix
    INTEGER(1), PARAMETER :: verbose_results_l1=0          ! most useful stats
    INTEGER(1), PARAMETER :: verbose_results_l2=0           ! more additional stats (e.g. cyclical, not hp filtered)
    INTEGER(1), PARAMETER :: verbose_results_l3=0           ! even more additional stats (e.g. repeat mobility jf)
    INTEGER(1), PARAMETER :: verbose_distribution=0         ! distribution output in results section
    INTEGER(1), PARAMETER :: verbose_profileshift=0
    LOGICAL, PARAMETER :: verbose_progress= .true.

    INTEGER(1), PARAMETER :: test_switch=0

    
    !=================================
    ! FURTHER SWITCHES 
    !=================================
    
        INTEGER, PARAMETER :: marker_ind=1
        INTEGER, PARAMETER :: screenoutcapture_switch=0
        INTEGER, PARAMETER :: rouwenhorst_ind=0
        INTEGER, PARAMETER :: printtransitionmatrix_ind=1

        
        !=============================
        !  RESTRICTED/EXPANDED VERSIONS OF MODEL 
        !===============================
                        ! how to change the number of parameters, and the parameters taken into account
                        
                        ! 1) number of parameters: change in the configuration text file, for the command line
                        ! 2) change the number of parameters in the userfunction file
                        ! 3) change the version of the model run in modglobal_ctv_eff.f90
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! IMPORTANT IMPORTANT IMPORTANT IMPORTANT       
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! need to adjust the number of parameters and the fixed parameters fed into the program in
        !  USER_FUNCTION.F90
        ! as well!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        ! MAIN VERSIONS OF THE PROGRAM
        INTEGER, PARAMETER  :: cyclical_reallc_ind=0                ! reallocation costs cyclical or not? p*c or just c 
        INTEGER, PARAMETER  :: heterogenous_delta_ind=1             ! HETEROGENEOUS DELTA
        INTEGER, PARAMETER  :: noreall_backwinduct_ind=0            ! adjusted backwards induction, skips reallocation step
        
        ! ADDITIONAL VERSIONS OF THE PROGRAM 
        INTEGER, PARAMETER  :: nosearch_backwinduct_ind=0              ! adjusted backwards induction, skips theta determination step
        INTEGER, PARAMETER  :: nosearch_withvacancy_ind=0            ! in the competitive case, this means that each hire is created by exactly one vacancy.
   
            
        ! HOW THETA GETS TO PARAMETERS
        ! ---- IMPORTANT --- STILL NEED TO CHANGE NUMBER OF PARAMETERS IN USER_FUNCTION.F90-------
              ! SET_B: still set in user_function, through theta 
        INTEGER, PARAMETER :: standard_theta_to_pars_ind=1            ! feed 13-14 parameter theta to the parameters, the standard way
        
        ! PARAMETER ASSIGNMENT HANDLED IN USER_FUNCTION. NEED TO ADJUST MOMENTS CALCULATION THOUGH 
        !INTEGER, PARAMETER :: pars6_nohcnos_setb_ind=0                   ! additional condition: in addition: no search parameters and possibly adapted targets 
        !INTEGER, PARAMETER :: pars7_nohcnos_estb_ind=0                   ! additional condition: in addition: no search parameters and possibly adapted targets 
        !INTEGER, PARAMETER :: pars9_nohcsearch_setb_ind=0                   ! additional condition: in addition: SEARCH parameters k, eta, delta and possibly adapted targets 
        !INTEGER, PARAMETER :: pars10_nohcsearch_estb_ind=0                   ! additional condition: in addition: SEARCH parameters k, eta, delta and possibly adapted targets 
        !
        
        !===========================================================================
        ! WHICH MOMENTS USED IN THE ESTIMATION (ONLY ONE OF THE BELOW OPTIONS!!!!)
        !===========================================================================
        
        INTEGER, PARAMETER :: moments_fullversion_ind=0                             ! includinig limited set of age group targets
        INTEGER, PARAMETER :: moments_lifecycleversion_ind=0                        ! all targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_bevcurve_ind=0               ! add beveridge curve to the above lifecycle moments
        INTEGER, PARAMETER :: moments_lifecycleversion_durshift_ind=0               ! add durationshift to the above.
        INTEGER, PARAMETER :: moments_lifecycleversion_durshiftslope_ind=0          ! durationshift, but target the slopes of the p33 and p67 profiles
        INTEGER, PARAMETER :: moments_lifecycleversion_bevcurve_durshift_ind=0      ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_bevcurve_durshiftslope_ind=0 ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_umedian_ind=0               ! add beveridge curve to the above lifecycle moments
        INTEGER, PARAMETER :: moments_lifecycleversion_umedian_durshift_ind=0      ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_umedian_durshiftslope_ind=0 ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_bev_umedian_ind=0               ! add beveridge curve to the above lifecycle moments
        INTEGER, PARAMETER :: moments_lifecycleversion_bev_umedian_durshift_ind=0      ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        INTEGER, PARAMETER :: moments_lifecycleversion_bev_umedian_durshiftslope_ind=0 ! add beveridge curve to durshift and above, targets incl. full set of life cycle 
        
        INTEGER, PARAMETER :: moments_lifecycleversion_durshift_jan2020_ind=0 ! no mueller moment, no umedian, no rel u y/p
        
        ! additional moment: andy mueller moment
        INTEGER, PARAMETER :: moments_mueller_ind=0
        
        ! all moments as before, BUT NO REALLOCATION
        INTEGER, PARAMETER :: moments_lifecycleversion_umedian_noreall_ind=0              ! NO REALL
        INTEGER, PARAMETER :: moments_lifecycleversion_bev_umedian_noreall_ind=0              ! add beveridge curve to the above lifecycle moments

        !no human capital, but with search 
        INTEGER, PARAMETER :: moments_nohc_setb_ind=0                  ! additional condition: no additional mM or wage disp differential target
        INTEGER, PARAMETER :: moments_nohc_estmm_ind=0                  ! additional condition: no additional mM or wage disp differential target
        INTEGER, PARAMETER :: moments_nohc_estdispdiff_ind=0                  ! additional condition: no additional mM or wage disp differential target
        
        ! no human capital and no search
        INTEGER, PARAMETER :: moments_nohcnos_setb_ind=0              ! additional condition: set b, so we don't estimate it, no searchy moments: no empirical eta 
        INTEGER, PARAMETER :: moments_nohcnos_nomm_nodiffdisp_ind=0                   ! additional condition: in addition: no search parameters and possibly adapted targets
        INTEGER, PARAMETER :: moments_nohcnos_estmm_ind=0
        INTEGER, PARAMETER :: moments_nohcnos_estcycle_ind=0                ! target rel volatility of reallocation rel. to productivity
        INTEGER, PARAMETER :: moments_nohcnos_estdiffdisp_ind=0
        !INTEGER, PARAMETER :: moments_nohcnos_setb_ind=0                   ! additional condition: in addition: no search parameters and possibly adapted targets 
                                                                                ! (e.g. el match function. Baseline: leave it as a target!) 
        
        
        
                ! targets when exlcuding reallocation moments 
        
        INTEGER, PARAMETER :: moments_nosearch_noreall_ind=0
        INTEGER, PARAMETER :: moments_nosearch_noreall_disp_ind=0
        INTEGER, PARAMETER :: moments_noreall_lifecycleversion_umedian_ind=0            ! shut down reallocation, but keep all age survival moments, human capital
        

        !INTEGER, PARAMETER :: restricted_to_earlier_employed=0         ! in measures of unemployed, only count those who have earlier employment in their lives.
        !                                                                ! this matters especially when thinking about the permanent heterogeneity model 
        !                                                                ! but we also have to keep an eye on this, keeping consistency of model and data 



        ! information display moments
        INTEGER, PARAMETER :: durshift_stats_ind=1
        INTEGER, PARAMETER :: print_regress_errmsg_ind=1
        INTEGER, PARAMETER :: always_print_iteration_ind=0


        ! NUN CALIBRATION
        INTEGER, PARAMETER :: nun_calib_no_umed_moment_ind=0


!************************************
!Subroutines and functions
!************************************


CONTAINS

SUBROUTINE nrerror(string)
    ! Report the error message, then stop the program (from NRUTIL, num recipes)
    CHARACTER(LEN=*), INTENT(IN) :: string
    WRITE (*,*) '!!!!!!!!!!!!! nrerror: ', string
    STOP 'program termined by nrerror'
END SUBROUTINE nrerror


!SUBROUTINE test123(tempval)
!    REAL(8) :: tempval
!    tempval=1
!end subroutine  test123

!SUBROUTINE sub_unirand(draws,vector)
!
!    INTEGER, INTENT(in) :: draws
!    REAL(8), DIMENSION(:), INTENT(inout) :: vector

	!CALL rnset(seed)		!Don't want to set seed, b/c then every simulation is exactly the same!!
	!CALL rnun(vector)

!END SUBROUTINE sub_unirand

!===========================================
! SUBSORT
!===========================================

SUBROUTINE sub_sort2(arr, bmat)

   REAL(8), DIMENSION(:), INTENT(inout) :: arr
   REAL(8), DIMENSION(:,:), INTENT(inout) :: bmat
   REAL(8) :: a
   REAL(8), DIMENSION(size(bmat,dim=1)) :: b
   INTEGER :: i, j

   DO j=2,size(arr)
       a=arr(j)
       b=bmat(:,j)
       DO i=j-1,1,-1
           IF (arr(i) <= a) EXIT
           arr(i+1) = arr(i)
           bmat(:,i+1)=bmat(:,i)
       ENDDO
       arr(i+1)=a
       bmat(:,i+1)=b
   ENDDO

END SUBROUTINE sub_sort2


SUBROUTINE sub_sort2_3first(arr, bmat)

   REAL(8), DIMENSION(:,:), INTENT(inout) :: arr                 ! program feeds in (npar_local+1, 3), where 3 is the number of distances calculated in hierarchical SMM
   REAL(8), DIMENSION(:,:), INTENT(inout) :: bmat              ! program feeds in (number of parameters to SMM over, number of parameter tuples)
   REAL(8) :: a
   REAL(8), DIMENSION(size(arr, dim=2)) :: avector
   REAL(8), DIMENSION(size(bmat,dim=1)) :: b
   INTEGER :: i, j

   DO j=2,size(arr, dim=1)   ! from 2 to npar_local+1 (so j is an index of a parameter tuple)
       a=arr(j,1)                      ! value of distance (in this case first distance), for parameter tuple indexed by j
       avector=arr(j,:)              ! entire vector of distances, for parameter tuple indexed by j
       b=bmat(:,j)                    ! tells us the actual parameter vector for parameter tuple indexed by j
       DO i=j-1,1,-1
           IF (arr(i,1) <= a) EXIT         ! if distance 1 of the i-tuple of parameters is smaller than the j-tuple of parameters exit and move on
           arr(i+1, :) = arr(i,:)                 !     otherwise move i-tuple to i+1 coordinate
           bmat(:,i+1)=bmat(:,i)
       ENDDO
       arr(i+1,:)=avector
       bmat(:,i+1)=b
   ENDDO

END SUBROUTINE sub_sort2_3first

SUBROUTINE sub_sort2_second(arr, bmat)

   REAL(8), DIMENSION(:,:), INTENT(inout) :: arr
   REAL(8), DIMENSION(:,:), INTENT(inout) :: bmat
   REAL(8) :: a
   REAL(8), DIMENSION(size(arr, dim=2)) :: avector
   REAL(8), DIMENSION(size(bmat,dim=1)) :: b
   INTEGER :: i, j

   DO j=2,size(arr, dim=1)
       a=arr(j,2)
       avector=arr(j,:)
       b=bmat(:,j)
       DO i=j-1,1,-1
           IF (arr(i,2) <= a) EXIT
           arr(i+1, :) = arr(i,:)
           bmat(:,i+1)=bmat(:,i)
       ENDDO
       arr(i+1,:)=avector
       bmat(:,i+1)=b
   ENDDO

END SUBROUTINE sub_sort2_second


! SORTING THE TUPLES AND DISTANCES FOR THE HIGHEST STAGE OF HIERARCHICAL SMM

SUBROUTINE sub_sort2_top(arr, bmat)

   REAL(8), DIMENSION(:,:), INTENT(inout) :: arr
   REAL(8), DIMENSION(:,:), INTENT(inout) :: bmat
   REAL(8) :: a
   REAL(8), DIMENSION(size(arr, dim=2)) :: avector
   REAL(8), DIMENSION(size(bmat,dim=1)) :: b
   INTEGER :: i, j

   DO j=2,size(arr, dim=1)
       a=arr(j,3)
       avector=arr(j,:)
       b=bmat(:,j)
       DO i=j-1,1,-1
           IF (arr(i,3) <= a) EXIT
           arr(i+1, :) = arr(i,:)
           bmat(:,i+1)=bmat(:,i)
       ENDDO
       arr(i+1,:)=avector
       bmat(:,i+1)=b
   ENDDO

END SUBROUTINE sub_sort2_top




!-----------------------------------------------------------
!                NORMAL DISTRIBUTION: density function
!-------------------------------------------------------------

REAL(8) FUNCTION ndistr(x, sigma)           ! mean =1    !
REAL(8), INTENT(IN) :: x, sigma
    ndistr=(1.0_8/(sigma*2.50662827_8))*EXP(- (((x-1)**2)/(2*(sigma**2))) )
END FUNCTION

!-----------------------------------------------------------------
!               NORMAL DISTRIBUTION: cdf
!-----------------------------------------------------------------

REAL(8) FUNCTION NORM_CDF(x, mu, sigma)
REAL(8), INTENT(IN) :: x, mu, sigma
     NORM_CDF=0.5_8*(1.0_8+DERF((x-mu)/(sqrt(2.0_8*sigma**2.0_8))))
END FUNCTION


!-----------------------------------------------------------------------------
!               LOG NORMAL DISTRIBUTION
!-----------------------------------------------------------------------------

REAL(8) FUNCTION LNORM(x, mu, sigma)
REAL(8), INTENT(IN) :: x, mu, sigma
     LNORM=0.5_8*(1.0_8+DERF((LOG(x)-mu)/(sqrt(2.0_8*sigma**2.0_8))))
END FUNCTION




!--------------------------------------------
!  MULTIVARIATE REGRESSION
!------------------------------------------

! X is n * number of regressors
! Y is n * 1

SUBROUTINE multivar_regression(Y, X, nsize, number_of_reg, reg_coeff, errmsg)

!-----------------------------------------------------------------------
! Calculate the regression coefficient based on the normal equation:
!    reg_coeff=(XTX)^(-1)*XT*Y
! where
!   X(nsize,number_of_reg) ... independent variable matrix (input)
!   Y(nsize,1) ... dependent variable matrix (input)
!   XT(number_of_reg,nsize) ... transpose of X
!
!Programming Note: Use GAUSSJ of Numerical Recipes to invert a square matrix XT*X
!-----------------------------------------------------------------------
!Link this+GAUSSJ;
!-----------------------------------------------------------------------


INTEGER(4), INTENT(in) :: nsize 			! number of observations
REAL(8), DIMENSION(:,:), INTENT(in) ::  Y		! dependent variable observations
REAL(8), DIMENSION(:,:), INTENT(in) :: X		! independent variable observations
INTEGER(4), INTENT(in) :: number_of_reg		! number of regressors, INCLUDING CONSTANT	

REAL(8), DIMENSION(number_of_reg,1), INTENT(OUT) :: reg_coeff		! DEFINED as a matrix
INTEGER(4), INTENT(out) :: errmsg

REAL(8), DIMENSION(number_of_reg, nsize) :: XT
REAL(8), DIMENSION(number_of_reg, number_of_reg) :: XTX
REAL(8), DIMENSION(number_of_reg, 1) :: XTY
REAL(8), DIMENSION(number_of_reg, number_of_reg) :: XTX_inv

!

!errmsg=0
! CHECK SIZES

IF (SIZE(X, DIM=1) .ne. nsize) THEN
    WRITE(*,*) 'INCONGRUOUS SIZES MATRIXES IN MULTIVAR REGRESSION - X DIM 1, press any key'
    PAUSE
END IF
IF (SIZE(X, DIM=2) .ne. number_of_reg) THEN
    WRITE(*,*) 'INCONGRUOUS SIZES MATRIXES IN MULTIVAR REGRESSION - X DIM 2, press any key'
    PAUSE
END IF
IF (SIZE(Y, DIM=1) .ne. nsize) THEN
    WRITE(*,*) 'INCONGRUOUS SIZES MATRIXES IN MULTIVAR REGRESSION - Y DIM 1, press any key'
    PAUSE
END IF
IF (SIZE(Y, DIM=2) .ne. 1) THEN
    WRITE(*,*) 'INCONGRUOUS SIZES MATRIXES IN MULTIVAR REGRESSION - Y DIM 2, press any key'
    PAUSE
END IF

! TRANSPOSE MATRIX

XT=TRANSPOSE(X)
XTX=MATMUL(XT, X)
XTY=MATMUL(XT, Y)

! INVERT XTX

reg_coeff=1				! just using reg_coeff as an irrelevant placeholder
XTX_inv=XTX

CALL GAUSSJ(XTX_inv, reg_coeff, errmsg)


! do the final multiplication

reg_coeff=MATMUL(XTX_inv, XTY)


END SUBROUTINE multivar_regression


!------------------------------------------------------------------
!  I N V E R T I N G   A   M A T R I X
!------------------------------------------------------------------



SUBROUTINE gaussj(a,b, errmsg)

! adapted from NR
! note: we do not check matrix dimensions here, as they are checked in the inputting program


IMPLICIT NONE
REAL(8), DIMENSION(:,:), INTENT(INOUT) :: a,b
INTEGER(4), INTENT(OUT) :: errmsg
			! a is an N x N input coefficient matrix, b is an N x M input matrix containing M RHS vectors
			! on output, a is replaced by its inverse, and b by the set of solution vectors

INTEGER(4), DIMENSION(SIZE(a,1)) :: ipiv, indxr, indxc		! bookkeeping on the pivoting
LOGICAL, DIMENSION(SIZE(a,1)) :: lpiv
REAL(8) :: pivinv
REAL(8), DIMENSION(SIZE(a,1)) :: dumc
INTEGER(4), TARGET :: irc(2)
INTEGER(4) :: i,l,n
INTEGER(4), POINTER :: irow, icol


! can check the SIZE of the matrices here

!IF(SIZE(a, dim=1) .ne. SIZE(a, dim=2)) !!STOP 'square matrix not square, in matrix inversion, pause'
errmsg=0

n=SIZE(a, dim=1)			! set matrix dimensions


irow => irc(1)
icol => irc(2)
ipiv=0

Do i=1,n
	
	!!---------- begin search for pivot element

	lpiv=(ipiv==0)
	
	irc=maxloc(abs(a), outerand(lpiv, lpiv))			! outerand(lpiv, lpiv) is the mask over matrix a. Initially entire matrix a is considere
	ipiv(icol)=ipiv(icol)+1							! note that maxloc returns (irow, icol) of the max.

	IF (ipiv(icol)>1) THEN
		!WRITE(*,*) 'GAUSSJ: singular matrix'
        errmsg=1
		!PAUSE
	END IF

	! We now have the pivot element, so we interchange rows, if needed, to put the pivot element on the diagonal
	!  The columns are                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             not physically interchanged, only relabel: indxc(i), the column of the ith pivot element, is
	!  is the ith column tha t is reduced, while indxr(i) is the row in which that pivot element was originally located. If indxr(i)
	!  does not equal indxc(i) there is an implied column interchange. With this form of bookkeeping, the solution b's will end up in the correct order
	!  and the inverse matrix will be scrambled by columns


	IF (irow /= icol) THEN
		CALL swap_rv(a(irow,:), a(icol,:))
		CALL swap_rv(b(irow,:), b(icol,:))
	END IF						! --- now the pivot element should be on the diagonal (icol, icol)



	!! ---- Now we divide the pivot row by the pivot element, located at irow, icol

	indxr(i)=irow
	indxc(i)=icol

	IF(a(icol, icol)==0.0_8)	THEN 		
		!WRITE(*,*) 'GAUSSJ: singular matrix (2nd test)'
		!PAUSE
        errmsg=2
	END IF

	pivinv=1.0_8/a(icol,icol)
	a(icol,icol)=1.0_8
	a(icol,:)=a(icol,:)*pivinv
	b(icol,:)=b(icol,:)*pivinv
	dumc=a(:,icol)					! icol-column of a matrix, which includes a(icol, icol) naturally, is assigned to dumc
	a(:, icol)=0.0_8				! set the column that was just assigned to zero.

	!! ----- reducing the rows, except for the pivot one

	a(icol, icol)=pivinv							! change a(icol,icol) to 1.0_8/old a(icol, icol)
	a(1:icol-1, :)=a(1:icol-1,:)-outerprod(dumc(1:icol-1), a(icol,:))
	b(1:icol-1, :)=b(1:icol-1,:)-outerprod(dumc(1:icol-1), b(icol,:))
	a(icol+1:,:)=a(icol+1:, :)-outerprod(dumc(icol+1:), a(icol,:))
	b(icol+1:,:)=b(icol+1:, :)-outerprod(dumc(icol+1:), b(icol,:))

   END DO 			! pivoting loop

!!----Unscrambling the column switches

DO l=n, 1, -1

	CALL swap_rv(a(:, indxr(l)),a(:, indxc(l)))
END DO


END SUBROUTINE gaussj


!--------------------------------------------------
!   AUXILIARY FUNCTIONS FROM NRUTIL
!--------------------------------------------------		

FUNCTION outerand(a,b)
LOGICAL, DIMENSION(:), INTENT(IN) :: a,b
LOGICAL, DIMENSION(SIZE(a), SIZE(b)) :: outerand

outerand=spread(a, dim=2, ncopies=SIZE(b)) .and. spread(b, dim=1, ncopies=SIZE(a))

END FUNCTION outerand


!!------ outerprod(a,b)
!!	creates a matrix m where m(i,j)=a(i) times b(j)


FUNCTION outerprod(a,b)

REAL(8), DIMENSION(:), INTENT(IN) :: a,b
REAL(8), DIMENSION(SIZE(a), SIZE(b)) :: outerprod

outerprod=spread(a, dim=2, ncopies=SIZE(b)) * spread(b, dim=1, ncopies=SIZE(a))

END FUNCTION outerprod


SUBROUTINE swap_i(a,b)
INTEGER(4), INTENT(INOUT) :: a,b
INTEGER(4) :: dum

dum=a
a=b
b=dum
END SUBROUTINE swap_i


SUBROUTINE swap_rv(a,b)

REAL(8), DIMENSION(:), INTENT(INOUT) :: a,b
REAL(8), DIMENSION(SIZE(a)) :: dum

dum=a
a=b
b=dum
END SUBROUTINE swap_rv





END MODULE modglobal_ctv_eff
