
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

!! VERSION JANUARY 2023  
    
!----------------------------------------------------------------------*****
!Carrillo-Tudela and Visschers 
!
!    !    !  the BACKWARDS INDucTION
!
!--------------------------------------------------------------------------------------------------******
!                                                                           T I M I N G
!
!
!           ----------|I|-------------------|------------------------------|-----------------------------|------------------------|------------------------------|II|------
!                           beginning of period                REALLOCATION                                        PRODUCTION, PAYMENTS
!                                            BREAKUP DECISIONS                          SEARCH AND MATCHING                                    beginning of next period
!                                             ~~newly unemployed CANNOT search
!                                           experience increase is realized
!--------------------------------------------------------------------------------------------------******


    
!include 'mkl_vsl.f90'
!include "errcheck.inc"


MODULE mod_backward_ctv

!USE cmaes_param_mod
USE ctv_grid_mod
USE modglobal_ctv_eff

!USE mod_solve_ctv
!USE mod_tauchen_ctv         ! TAUCHEN DISCRETICIZATION
!USE omp_lib
!USE ifport
!USE mpif.h

IMPLICIT NONE

    CONTAINS

        !------------------------------------
        ! BACKWARDS INDUCTION
        !------------------------------------


    SUBROUTINE ctv_backwards_induct_nm(VEl, VUl, zrl, zsl, poliDWl, poliRl, jfl, wagel, &
            VElo, VUlo, zrlo, zslo, poliDWlo, poliRlo, jflo, wagelo, searchdir_lo, &
            prodl, ztransmatrixl, ptransmatrixl, xtransmatrixl, znewpdfl, &
            threadnol, skilldepl, pvector_in, zvector_in, xvector_in, &
            deltalowhcl, deltahighhcl, &
            occprodl, occaggprod_elasl, searchdir_parsl, occsearch_elasl &
            )
            ! ADDITIONAL PARAMETERS IN THIS BACKWARDS INDUCTION
            !           occupation-specific productivities (3, 1 normalized)
            !           occupation-specific elasticities to the cycle (3, 1 normalized)
            !           searchdir_parameters (16, I think), but add up to 1, so reaseally 12
            !           searchelasticity (1 par)

            !! EXCESS MOBILITY MODEL
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VEl
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VUl
            INTEGER, DIMENSION(:,:), INTENT(inout) :: zrl
            INTEGER, DIMENSION(:,:), INTENT(inout) :: zsl
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: poliDWl
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: poliRl
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: jfl
            !! OCCUPATION WIDE
            REAL(8), DIMENSION(:,:,:,:), INTENT(inout) :: VElo
            REAL(8), DIMENSION(:,:,:,:), INTENT(inout) :: VUlo
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: zrlo
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: zslo
            INTEGER, DIMENSION(:,:,:,:), INTENT(inout) :: poliDWlo
            INTEGER, DIMENSION(:,:,:,:), INTENT(inout) :: poliRlo
            REAL(8), DIMENSION(:,:,:,:), INTENT(inout) :: jflo
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: Searchdir_lo

            REAL(8), DIMENSION(ppts), INTENT(in) :: pvector_in
            REAL(8), DIMENSION(xpts), INTENT(in) :: xvector_in
            REAL(8), DIMENSION(zpts), INTENT(in) :: zvector_in
            REAL(8), INTENT(IN) :: deltahighhcl, deltalowhcl
            INTEGER, INTENT(IN) :: threadnol

            !REAL(8), DIMENSION(:), INTENT(in): zvector
            !REAL(8), DIMENSION(:), INTENT(in): xvector

            REAL(8), DIMENSION(ppts,xpts,zpts), INTENT(inout) :: wagel
            REAL(8), DIMENSION(occpts, ppts,xpts,zpts), INTENT(inout) :: wagelo

            REAL(8), DIMENSION(ppts,xpts,zpts), INTENT(in):: prodl
            REAL(8), DIMENSION(ppts,ppts), INTENT(in):: ptransmatrixl
            REAL(8), DIMENSION(zpts,zpts), INTENT(in):: ztransmatrixl

            REAL(8), DIMENSION(xpts,xpts), INTENT(in):: xtransmatrixl

            REAL(8), DIMENSION(zpts), INTENT(in) :: znewpdfl
            REAL(8), INTENT(IN) :: skilldepl

            REAL(8), DIMENSION(occpts), INTENT(IN)          :: occprodl
            REAL(8), DIMENSION(occpts), INTENT(IN)          :: occaggprod_elasl
            REAL(8), DIMENSION(occpts, occpts), INTENT(IN)  :: searchdir_parsl
            REAL(8), INTENT(IN)                             :: occsearch_elasl

            REAL(8), DIMENSION(zpts,zpts):: ztranscdfl

            REAL(8), DIMENSION(ppts,ppts):: ptransmatrixl12
            REAL(8), DIMENSION(zpts,zpts):: ztransmatrixl12
            REAL(8), DIMENSION(xpts,xpts):: xtransmatrixl12
            REAL(8), DIMENSION(zpts,zpts):: ztranscdfl12

            REAL(8), DIMENSION(ppts) :: pveclocal
            REAL(8) :: skillapprl, skilldepl12
            REAL(8) :: btl12
            REAL(8), DIMENSION(xpts) :: delta_vector, btl12_vector

            CHARACTER*24            :: filename, x1,x2

            REAL(8), DIMENSION(occpts, ppts, xpts, zpts) :: prodlo
            REAL(8)                          :: VB

            ! INPUTS-OUTPUTS  **INOUT**
            !           - VE  (initial guess)
            !           - VU  (initial guess)
            !           - zr
            !           - zs
            !           - poliDW
            !           - poliR
            !           - JF
            !           - switch to calculate


    ! LOCALS
    INTEGER :: runnumber, runnumber_nm
    INTEGER :: zcnt, pcnt, p2cnt, xcnt, z2cnt, x2cnt,cntr, howard_no, tempint, occcnt, tempint2
    REAL(8) :: convdistance, convdistance2, convdistance_pol, convdistance3
    REAL(8) :: thetatemp, tempreal, tempreal2, tempreal3, tempreal4, tempreal5, tempreal6, tempreal7, tempreal8, tempnorm

    REAL(8), DIMENSION(ppts,xpts,zpts) :: DmaxUl, VUltemp, VEltemp
    REAL(8), DIMENSION(occpts, ppts,xpts,zpts) :: DmaxUlo, VUlotemp, VElotemp

    REAL(8), DIMENSION(ppts) :: reallocl
    REAL(8), DIMENSION(occpts, ppts) :: realloclo, realloclo_temp
    REAL(8), DIMENSION(occpts, ppts) :: temp_R_destocclo
    REAL(8), DIMENSION(occpts, ppts) :: temp_ERo_baseline
    REAL(8), DIMENSION(occpts, occpts, ppts) :: temp_occsearchchoice_matrix

    
    REAL(8) :: t11, t22
    INTEGER, PARAMETER :: test_verbose_ind=0


    !! net mob reallocation optimization
    INTEGER :: lambdaiter
    REAL(8) :: lambdamax, lambdamin,lambdasave,lambdasol, flambdamax, flambdamin
    REAL(8), DIMENSION(occpts, ppts) :: lambdasol_opvector
    !***************************************************************
    ! 0. SET UP AUXILIARY VARS
    !***************************************************************

    IF(cyclical_reallc_ind .ne. 1) THEN
                pveclocal=1.0_8
    ELSE IF (cyclical_reallc_ind .eq. 1) THEN
                pveclocal=pvector_in


    END IF

    IF(test_verbose_ind==1) THEN
                    WRITE(*,*) 'pveclocal'
                    WRITE(*,*) pveclocal
    END IF

    IF(xpts==1) THEN
        !WRITE(*,*) 'NO SKILL APPRECIATION'
        !STOP
        skillapprl=0.0_8
    END IF
    skillapprl=0.0_8
    IF(xpts>1) THEN
        skillapprl=xtransmatrixl(1,2)
    END IF


    IF(heterogenous_delta_ind==1) THEN
        ! THIS CAN OCCUR BY CHANCE, SO DO NOT STOP
        !IF(deltahighhcl .eq. deltalowhcl) STOP 'HETEROGENEOUS DELTA CONTRADICTION deltalhcl==deltahhcl'
         delta_vector(1)=deltalowhcl
        IF(xpts>1) delta_vector(2:xpts)=deltahighhcl
    ELSE
        IF(deltalowhcl .ne. deltahighhcl) STOP 'HOMOGENOUS DELTA CONTRADICTION deltalhcl!=deltahhcl'
    END IF

    !-----------------------------
    ! construct 12 weeks ahead discount rate, and matrices
    !-----------------------------
    howard_no=12

    btl12=(bt*  (1.0_8-dd))**howard_no

    ptransmatrixl12=ptransmatrixl
    ztransmatrixl12=ztransmatrixl
    xtransmatrixl12=xtransmatrixl


   tempint = SIZE(ptransmatrixl,1)
   ptransmatrixl12 = 0.0
   DO cntr=1,tempint
     ptransmatrixl12(cntr,cntr) = 1.0
   ENDDO

   tempint = SIZE(ztransmatrixl,1)
   ztransmatrixl12 = 0.0
   DO cntr=1,tempint
     ztransmatrixl12(cntr,cntr) = 1.0
   ENDDO

   tempint = SIZE(xtransmatrixl,1)
   xtransmatrixl12 = 0.0
   DO cntr=1,tempint
     xtransmatrixl12(cntr,cntr) = 1.0
   ENDDO



   DO cntr=1, howard_no
    ptransmatrixl12=MATMUL(ptransmatrixl12, ptransmatrixl)
    END DO

    DO cntr=1, howard_no
    ztransmatrixl12=MATMUL(ztransmatrixl12, ztransmatrixl)
    END DO


    IF(xpts>1) THEN
        DO cntr=1, howard_no
        xtransmatrixl12=MATMUL(xtransmatrixl12, xtransmatrixl)
        END DO
    END IF
!----------
! construct ztranscdf, ztranscdfl12
!---------

ztranscdfl(:,1)=ztransmatrixl(:,1)

DO zcnt=1,zpts
    DO z2cnt=2,zpts
        ztranscdfl(zcnt,z2cnt)=ztranscdfl(zcnt,z2cnt-1)+ztransmatrixl(zcnt,z2cnt)
    END DO

END DO
!WRITE(*,*) 'ztranscdfl at highest point, should equal 1', ztranscdfl(:,zpts)


ztranscdfl12(:,1)=ztransmatrixl12(:,1)

DO zcnt=1,zpts
    DO z2cnt=2,zpts
        ztranscdfl12(zcnt,z2cnt)=ztranscdfl12(zcnt,z2cnt-1)+ztransmatrixl12(zcnt,z2cnt)
    END DO

END DO

!! SET UP PRODUCTIVITY MATRIX WITH OCCUPATION-WIDE PRODUCTIVITIES

DO xcnt=1, xpts
    DO zcnt=1, zpts
        DO occcnt=1, occpts
            DO pcnt=1, ppts
                    !tempreal=   OPROD_FUNC(occcnt, pcnt)
                    prodlo(occcnt, pcnt, xcnt, zcnt)=OPROD_FUNC(occcnt, pcnt)*xvector_in(xcnt)*zvector_in(zcnt)
            END DO
        END DO
    END DO
END DO



heterogenous_delta_if: &
IF (heterogenous_delta_ind .ne. 1) THEN
    !**************************************************************
    !
    !I. Solve the model backwards
    !
    !**************************************************************

VUltemp=0.0_8
VEltemp=0.0_8
tempreal2=0.0_8
tempreal3=0.0_8
tempreal4=0.0_8
tempreal5=0.0_8
tempreal6=0.0_8
tempreal7=0.0_8
tempreal8=0.0_8


    !--------------------------------------------------------------
    !   LAST PERIOD + HOWARD's IMPROVEMENT ALGORITHM-LIKE ITERATIONS
    !--------------------------------------------------------------

            ! some approximations here, that lead to faster convergence to a good enough 'guess' input in the second step where iterations proceed without approximations


    IF(verboseswitch==1) WRITE(*,*) "Starting to solve the model"

!FOR EACH IS-realization, do step 1+2


    runnumber=0
    convdistance=10000

    IF (threadnol==1 .AND. verbose_backw_ind==1) THEN
    WRITE(10,*) 'beginning of convergence loop', runnumber, maxrunnumber, convdistance, convcrit
    END IF



convergence_loop_qtr: &
    DO WHILE (convdistance>convcrit .AND. runnumber<MIN(500.0, maxrunnumber/2.0))

    runnumber=runnumber+1
    IF (verbose_backw_ind==1 .AND. runnumber==2) WRITE(*,*) 'entering quarterly convergence loop'




                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 1)  DETERMINING  TH(z,t), Dmax(t, V), DmaxUl(t)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                            ! Here we use the planner's problem:
                            !
                            !
                            !  FOC of  maximizing the surplus value of the application: k=eta*TH**eta-1 [EV(tmax)-V(zcnt,.tmax)]
                            !                   from  (1) k=q(TH) [EV - X], where X is the value applied to by the worker
                            !                    and   (2) Dmax(t,V)=max p(TH)[X-V]
                            !                                           SOLVE FOR X, and take a derivative wrt to TH to find the solution

                            ! 4a. UNEMPLOYED JOB FINDING

                                IF(nosearch_backwinduct_ind .NE. 1 ) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      thetatemp=(   ( eta*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   )/k   )**( 1.0/(1.0-eta) )

                                                              IF(thetatemp>1) thetatemp=1.0_8

                                                      DmaxUl(pcnt, xcnt, zcnt)=pf(thetatemp)*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   -  k*thetatemp

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=pf(thetatemp)

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr
                                                    END IF
                                        END DO islandloop_qtr
                                     END DO
                                END DO

                                ELSE IF (nosearch_backwinduct_ind==1) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr2: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      DmaxUl(pcnt, xcnt, zcnt)=(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=1.0_8

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr2
                                                    END IF
                                        END DO islandloop_qtr2
                                     END DO
                                END DO
                                END IF

                                !IF(runnumber<=3) WRITE(*,*) DmaxUl(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 2)  REALLOCATION DECISION
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        IF(noreall_backwinduct_ind .ne. 1) THEN
                            IF (instantreall==0) THEN

                                DO pcnt=1, ppts
                                    tempreal6=0.0_8
                                            DO zcnt=1, zpts
                                                    ! this will be the continuation value after reallocating

                                                        tempreal6=znewpdfl(zcnt)*VUl(pcnt,1, zcnt) +tempreal6            ! figuring out E[REALLOC]
                                                                                                      ! reallocation choice    ! application choice                  ! NOTE, ZERO
                                                                                                      !TENURE IN OCCUPATION, SO NO HC
                                                                                                      ! reallocation choice    ! application choice


                                            END DO
                                            Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                END DO

                              ELSE IF (instantreall==1) THEN
                                    DO pcnt=1, ppts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                                        ! this will be the continuation value after reallocating

                                                            tempreal6=znewpdfl(zcnt)*(VUl(pcnt,1, zcnt)+DmaxUl(pcnt,1,zcnt)) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE,
                                                                                                          !ZERO TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice


                                                END DO
                                                Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                    END DO
                              END IF
        ELSE IF(noreall_backwinduct_ind == 1) THEN

                                Reallocl=-HUGE(1.0_8)

        END IF


!================================================
!  VALUE FUNCTION CALCULATION (implicit reallocation and separation decision)
!================================================





 IF(noreall_backwinduct_ind .ne. 1) THEN
                            do pcnt=1, ppts
                                do zcnt=1, zpts
                                    do xcnt=1, xpts


                                    !-------
                                    ! VU CALCULATION
                                    !-------


                                    ! expected skill depreciation over 12 weeks
                                    IF (skilldepl .ne. 0.0_8) THEN

                                        IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                            skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                            skilldepl12=MAX(MIN(1.0, skilldepl12),0.0_8)
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                            skilldepl12=skilldepl
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                            skilldepl12=MAX(MIN(1.0, 1.0-(1.0-skilldepl)**howard_no),0.0_8)
                                        END IF
                                    ELSE IF (skilldepl .eq. 0.0_8) THEN
                                        skilldepl12=0.0_8
                                    END IF

                                        tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4qtr: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 .AND. (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt==1 .AND. (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztranscdfl12(zcnt, z2cnt)* &
                                                      (REALLOCl(p2cnt))        +tempreal6
                                                    EXIT zloop_4qtr

                                                  ELSE  IF(xpts>1 .AND. xcnt>1) THEN ! xcnt>1

                                                      IF((REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                                                          (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)))  ) THEN

                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl12)* &
                                                                      (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                                                                    +skilldepl12* &
                                                                    (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                                                                    ) &
                                                                    +tempreal6
                                                      ELSE IF  ((REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                                                          (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)  ))) THEN


                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztranscdfl12(zcnt, z2cnt)* (REALLOCl(p2cnt))        &
                                                                    +tempreal6
                                                            EXIT zloop_4qtr
                                                        END IF
                                                END IF

                                                END DO zloop_4qtr
                                        END DO


                                    !-------
                                    ! VE CALCULATION
                                    !-------
                                        tempreal5=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal5=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)*xtransmatrixl12(xcnt, x2cnt)*&
                                                                        (delta*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta)*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal5
                                                END DO
                                            END DO
                                        END DO

                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VUltemp = b          *   (1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))                           + beta^12(1-dd)^12 (1-jf)^12 VU
                                    !                              =tempreal2
                                    !            y(p,x,z)   *   (1-btl12)/(1-beta(1-dd))-(1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                              =tempreal3           - tempreal2

                                    !-------
                                    ! HOWARD's IMPROVEMENT
                                    !-------



                                        IF (jfl(pcnt, xcnt, zcnt)>=0 .AND. jfl(pcnt, xcnt, zcnt)<1.0_8) THEN
                                            tempreal2=(1.0_8 - btl12*(1.0-jfl(pcnt, xcnt, zcnt))**howard_no)/(1.0_8-(1.0_8-jfl(pcnt, xcnt, zcnt))*bt*(1.0_8-dd))
                                            tempreal3=(1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                                ! tempreal6 = VU continuation value
                                                ! tempreal5 = VE continuation value
                                            VUltemp(pcnt, xcnt, zcnt)=b * tempreal2 &
                                                                        + (btl12*(1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no)*tempreal6 + &
                                                                      prodl(pcnt, xcnt, zcnt) * (tempreal3-tempreal2) &
                                                                        + btl12*(1.0-((1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no))*tempreal5
                                                ! note, we ignore the history where an unemployed worker finds a job and then loses it

                                         ! VU: compare to reallocation in week 1. Reallocate if profitable. (This has a imprecision of a week, but we ignore this as an
                                         !approximation)
                                            VUltemp(pcnt, xcnt, zcnt)=MAX(b + bt*(1.0_8-dd)*Reallocl(pcnt),VUltemp(pcnt, xcnt, zcnt))

                                        ELSE IF (jfl(pcnt, xcnt, zcnt)>=1.0) THEN
                                            VUltemp(pcnt, xcnt, zcnt)=b-prodl(pcnt,xcnt,zcnt)+ &
                                            ((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))*prodl(pcnt,xcnt,zcnt)+btl12*tempreal5
                                        ELSE
                                            VUltemp(pcnt, xcnt, zcnt)=b*((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))+btl12*tempreal6
                                        END IF


                                    ! VE, incorporate exogenous separations
                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VEltemp = y(p,x,z)          *   (1- ((1-delta)beta(1-dd))**12)/(1- (1-delta)beta(1-dd))       + beta^12(1-dd)^12 (1-delta)^12 VU
                                    !                                        =tempreal2
                                    !            b   *   (1-btl12)/(1-beta(1-dd))-(1- ((1-jf)beta(1-dd))**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                       =tempreal3           - tempreal2

                                                ! note we ignore the history where the worker loses a job and then finds it within the quarter. Here, this has more of a bite, as
                                                ! job finding rates are high enough to have a substantial amount of workers recover employment within the quarter. However,
                                                !exogenous
                                                ! separations themselves are still low, so overall the approximation is still helpful.
                                                ! to take this into account however, we use another rule simplification. Average duration is 1/jf, exogenous separation
                                                ! have on average 6 weeks. so survival in unemployment.
                                                ! tempreal4 captures the bouncing back into employment MIN(0.8,6/jf)

                                        !tempreal4= 1.0-MAX(MIN(0.8, 6.0/jfl(pcnt, xcnt, zcnt)),0.0_8)
                                        tempreal4=1.0_8
                                        tempreal2=  (1.0_8- btl12*((1.0_8-delta*tempreal4)**howard_no))/(1.0- (1.0-delta*tempreal4)*bt*(1.0-dd))
                                        tempreal3=  (1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                        VEltemp(pcnt, xcnt, zcnt) = prodl(pcnt, xcnt, zcnt)* tempreal2  &
                                                                        + (btl12*(1.0_8-delta*tempreal4)**howard_no)*tempreal5 + &
                                                                    b* (tempreal3-tempreal2)  &
                                                                        + btl12*(1.0-((1.0_8-delta*tempreal4)**howard_no))*tempreal6

                           !
                           !
                           !         ! VE, incorporate endogenous separation
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b + bt*(1.0_8-dd)*VUltemp(pcnt, xcnt, zcnt)),VEltemp(pcnt, xcnt, zcnt))
                           !
                           !         ! VE, compare to value of separation and reallocation. As this is just speeding up the convergence, we assume here that separations
                           !         !    lead to reallocation in the next period right away
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b*(1.0+bt*(1.0_8-dd)) + ((bt*(1.0_8-dd))**2)*Reallocl(pcnt)),VEltemp(pcnt, xcnt, zcnt))
                                    END DO
                                    END DO
                            END DO

 ELSE IF(noreall_backwinduct_ind == 1) THEN

                        do pcnt=1, ppts
                                do zcnt=1, zpts
                                    do xcnt=1, xpts


                                    !-------
                                    ! VU
                                    !-------


                                    ! expected skill depreciation over 12 weeks
                                    IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                        skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                        skilldepl12=MAX(MIN(1.0, skilldepl12), 0.0_8)
                                    ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                        skilldepl12=skilldepl
                                    ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                        skilldepl12=MAX(MIN(1.0, 1.0-(1.0-skilldepl)**howard_no), 0.0_8)
                                    END IF

                                        tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4qtr2: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 ) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt>1) THEN   ! xcnt>1

                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                                      ( &
                                                                    (1.0_8-skilldepl12)* &
                                                                      ((VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )       ) &
                                                                    +skilldepl12* &
                                                                    ((VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)     )   ) &
                                                                    ) &
                                                                    +tempreal6

                                                  END IF


                                                END DO zloop_4qtr2
                                        END DO


                                    !-------
                                    ! VE
                                    !-------
                                        tempreal5=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal5=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)*xtransmatrixl12(xcnt, x2cnt)*&
                                                                        (delta*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta)*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal5
                                                END DO
                                            END DO
                                        END DO

                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VUltemp = b          *   (1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))                           + beta^12(1-dd)^12 (1-jf)^12 VU
                                    !                              =tempreal2
                                    !            y(p,x,z)   *   (1-btl12)/(1-beta(1-dd))-(1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                              =tempreal3           - tempreal2


                                    !=============================
                                    ! Howard's improvement algorithm -type speed-up
                                    !=============================

                                        IF (jfl(pcnt, xcnt, zcnt)>=0 .AND. jfl(pcnt, xcnt, zcnt)<1.0_8) THEN
                                            tempreal2=(1.0_8 - btl12*(1.0-jfl(pcnt, xcnt, zcnt))**howard_no)/(1.0_8-(1.0_8-jfl(pcnt, xcnt, zcnt))*bt*(1.0_8-dd))
                                            tempreal3=(1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                                ! tempreal6 = VU continuation value
                                                ! tempreal5 = VE continuation value
                                            VUltemp(pcnt, xcnt, zcnt)=b * tempreal2 &
                                                                        + (btl12*(1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no)*tempreal6 + &
                                                                      prodl(pcnt, xcnt, zcnt) * (tempreal3-tempreal2) &
                                                                        + btl12*(1.0-((1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no))*tempreal5
                                                ! note, we ignore the history where an unemployed worker finds a job and then loses it

                                         ! VU: compare to reallocation in week 1. Reallocate if profitable. (This has a imprecision of a week, but we ignore this as an
                                         !approximation)
                                            VUltemp(pcnt, xcnt, zcnt)=MAX(b + bt*(1.0_8-dd)*Reallocl(pcnt),VUltemp(pcnt, xcnt, zcnt))

                                        ELSE IF (jfl(pcnt, xcnt, zcnt)>=1.0) THEN
                                            VUltemp(pcnt, xcnt, zcnt)=b-prodl(pcnt,xcnt,zcnt)+ &
                                            ((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))*prodl(pcnt,xcnt,zcnt)+btl12*tempreal5
                                        ELSE
                                            VUltemp(pcnt, xcnt, zcnt)=b*((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))+btl12*tempreal6
                                        END IF


                                    ! VE, incorporate exogenous separations
                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VEltemp = y(p,x,z)          *   (1- ((1-delta)beta(1-dd))**12)/(1- (1-delta)beta(1-dd))       + beta^12(1-dd)^12 (1-delta)^12 VU
                                    !                                        =tempreal2
                                    !            b   *   (1-btl12)/(1-beta(1-dd))-(1- ((1-jf)beta(1-dd))**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                       =tempreal3           - tempreal2

                                                ! note we ignore the history where the worker loses a job and then finds it within the quarter. Here, this has more of a bite, as
                                                ! job finding rates are high enough to have a substantial amount of workers recover employment within the quarter. However,
                                                !exogenous
                                                ! separations themselves are still low, so overall the approximation is still helpful.
                                                ! to take this into account however, we use another rule simplification. Average duration is 1/jf, exogenous separation
                                                ! have on average 6 weeks. so survival in unemployment.
                                                ! tempreal4 captures the bouncing back into employment MIN(0.8,6/jf)

                                        !tempreal4= 1.0-MAX(MIN(0.8, 6.0/jfl(pcnt, xcnt, zcnt)),0.0_8)
                                        tempreal4=1.0_8
                                        tempreal2=  (1.0_8- btl12*((1.0_8-delta*tempreal4)**howard_no))/(1.0- (1.0-delta*tempreal4)*bt*(1.0-dd))
                                        tempreal3=  (1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                        VEltemp(pcnt, xcnt, zcnt) = prodl(pcnt, xcnt, zcnt)* tempreal2  &
                                                                        + (btl12*(1.0_8-delta*tempreal4)**howard_no)*tempreal5 + &
                                                                    b* (tempreal3-tempreal2)  &
                                                                        + btl12*(1.0-((1.0_8-delta*tempreal4)**howard_no))*tempreal6

                           !
                           !
                           !         ! VE, incorporate endogenous separation
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b + bt*(1.0_8-dd)*VUltemp(pcnt, xcnt, zcnt)),VEltemp(pcnt, xcnt, zcnt))
                           !
                           !         ! VE, compare to value of separation and reallocation. As this is just speeding up the convergence, we assume here that separations
                           !         !    lead to reallocation in the next period right away
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b*(1.0+bt*(1.0_8-dd)) + ((bt*(1.0_8-dd))**2)*Reallocl(pcnt)),VEltemp(pcnt, xcnt, zcnt))
                                    END DO
                                    END DO
                            END DO





END IF

        !---------------------------------------------
        !  TEST FOR CONVERGENCE
        !---------------------------------------------

        tempreal6=MAXVAL(ABS(VUltemp(:,:,:)-VUl(:,:,:))/ABS(REAL(VUltemp(:,:,:))))
        tempreal5=MAXVAL(ABS(VEltemp(:,:,:)-VEl(:,:,:))/ABS(REAL(VEltemp(:,:,:))))

        convdistance=MAX(tempreal5,tempreal6)

        !IF(runnumber==2) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance
        !IF((runnumber/1000)*1000-runnumber==0) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance

        !IF(verboseswitch==1) WRITE(*,*) 'runnumber=', runnumber, ' convcrit= ', convdistance
        !IF(verboseswitch==1 .AND. (runnumber/10)*10-runnumber==0) WRITE(*,*) 'thr', threadnol, 'howardrunno.=', runnumber, ' dist=', convdistance


        VUl=VUltemp
        VEl=VEltemp

        IF(verbose_backw_ind==1 .AND. runnumber<=2) THEN
            WRITE(*,*) 'VEl(MAX(ppts/2, 1), 1, MAX(4*zpts)/5,1)', VEl(MAX((ppts/2), 1), 1, MAX((4*zpts)/5,1))
            WRITE(*,*) 'thr', threadnol, 'howardrunno.=', runnumber, ' dist=', convdistance
        END IF
            END DO convergence_loop_qtr


IF(verbose_backw_ind==1) THEN



    !WRITE(*,*) 'thread reporting', i1
WRITE(x1,FMT='(I3.3)') threadnol !

filename='backw_ind_howard'//trim(x1)//'.csv'
OPEN(UNIT=38, file=filename, form='formatted', status='replace')

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'p_index', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'x_index', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_index', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'p_level', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'x_level', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_level', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'prod', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'wage', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'VE', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'VU', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'poliDW', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'poliR', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_binary', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_binary', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_func', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_func', ','

write(38, FMT='(A1)' ) ','



    !                                        epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
    !                                        epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim
    !

DO pcnt=1,ppts
DO xcnt=1, xpts
DO zcnt=1, zpts

write(38, FMT='(I4, A1)' , ADVANCE='NO') pcnt, ','
write(38, FMT='(I4, A1)' , ADVANCE='NO') xcnt, ','
write(38, FMT='(I4, A1)' , ADVANCE='NO') zcnt, ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') pvector_in(pcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') xcnt, ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zcnt, ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') prodl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') wagel(pcnt,xcnt,zcnt), ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') VEl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') VUl(pcnt, xcnt, zcnt), ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') poliDWl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') poliRl(pcnt, xcnt,zcnt), ','

! EMPLOYMENT PER AGE
!! cutoffs
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliRl(pcnt,xcnt, 1)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliRl(pcnt,xcnt, zcnt-1)==0 .AND. poliRl(pcnt,xcnt, zcnt)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! separation cutoff
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliDWl(pcnt,xcnt, 1)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliDWl(pcnt,xcnt, zcnt-1)==0 .AND. poliDWl(pcnt,xcnt, zcnt)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! cutoff z-values (as a function of p,x)
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zrl(pcnt, xcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zsl(pcnt, xcnt), ','
write(38, FMT='(A1)' ) ','

END DO  !zcnt
END DO  !xcnt
END DO  ! pcnt

CLOSE(38)



END IF



 !=================================================================================
 !-------------------
 !  WEEKLY LOOP   MODEL
 !----------------------
 !==================================================================================



    VEltemp=0.0_8
    VUltemp=0.0_8
    convdistance2=10000



convergence_loop: &
    DO WHILE (convdistance2>convcrit .AND. runnumber<maxrunnumber)

    runnumber=runnumber+1
    IF (verbose_backw_ind==1 .AND. convdistance2==10000) WRITE(*,*) 'WEEKLY runxno is', runnumber, '  on thread ', threadnol

   !IF(verboseswitch==1) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        !IF(verboseswitch==1 .AND. (runnumber/100)*100-runnumber==0) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        IF(verbose_backw_ind==1 .AND. (runnumber/100)*100-runnumber==0) WRITE(*,*) 'runxnumber is', runnumber, '  on thread ', threadnol, 'conv distance is:', convdistance
       !IF((runnumber/1000)*1000-runnumber==0 .AND. threadnol==1) WRITE(*,*) 'runnumber=', runnumber

        IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) THEN
            !CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
            !WRITE(10,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            !WRITE(*,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            call cpu_time ( t22 )
            write ( *, * ) '1000 iteration BI: Elapsed CPU time = ', t22 - t11
            !write ( 10, * ) 'Within Backw Induction Elapsed CPU time = ', t22 - t11
            call cpu_time(t11)
        END IF



            nos_if: IF(nosearch_backwinduct_ind .NE. 1 ) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns2: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      thetatemp=(   ( eta*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   )/k   )**( 1.0/(1.0-eta) )

                                                              IF(thetatemp>1) thetatemp=1.0_8

                                                      DmaxUl(pcnt, xcnt, zcnt)=pf(thetatemp)*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   -  k*thetatemp

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=pf(thetatemp)

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns2
                                                    END IF
                                        END DO islandloop_qtr_ns2
                                     END DO
                                END DO

          ELSE IF (nosearch_backwinduct_ind==1) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      DmaxUl(pcnt, xcnt, zcnt)=(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=1.0_8

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns
                                                    END IF
                                        END DO islandloop_qtr_ns
                                     END DO
                                END DO
          END IF nos_if

                                !IF(runnumber<=3) WRITE(*,*) DmaxUl(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 2)  REALLOCATION DECISION
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        noreal_if: IF(noreall_backwinduct_ind .ne. 1) THEN
                            IF (instantreall==0) THEN

                                DO pcnt=1, ppts
                                    tempreal6=0.0_8
                                            DO zcnt=1, zpts
                                                    ! this will be the continuation value after reallocating

                                                        tempreal6=znewpdfl(zcnt)*VUl(pcnt,1, zcnt) +tempreal6            ! figuring out E[REALLOC]
                                                                                                      ! reallocation choice    ! application choice                  ! NOTE, ZERO
                                                                                                      !TENURE IN OCCUPATION, SO NO HC
                                                                                                      ! reallocation choice    ! application choice


                                            END DO
                                            Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                END DO

                              ELSE IF (instantreall==1) THEN
                                    DO pcnt=1, ppts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                                        ! this will be the continuation value after reallocating

                                                            tempreal6=znewpdfl(zcnt)*(VUl(pcnt,1, zcnt)+DmaxUl(pcnt,1,zcnt)) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE,
                                                                                                          !ZERO TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice


                                                END DO
                                                Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                    END DO
                              END IF
        ELSE IF(noreall_backwinduct_ind == 1) THEN

                                Reallocl=-HUGE(1.0_8)

        END IF noreal_if


                            !~~~~~~~~~~~~~~~~~
                            ! 4) New Unemployment value
                            !~~~~~~~~~~~~~~~~~

IF(noreall_backwinduct_ind .ne. 1) THEN


                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 .AND. (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt==1 .AND. (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* &
                                                      (REALLOCl(p2cnt))        +tempreal6
                                                    EXIT zloop_4

                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                      IF((REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                                                          (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)))  ) THEN

                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                                                                    +skilldepl* &
                                                                    (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                                                                    ) &
                                                                    +tempreal6
                                                      ELSE IF  ((REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                                                          (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)  ))) THEN


                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* (REALLOCl(p2cnt))        &
                                                                    +tempreal6
                                                            EXIT zloop_4
                                                        END IF
                                                END IF

                                                END DO zloop_4
                                        END DO
                                    VUltemp(pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
ELSE IF(noreall_backwinduct_ind == 1) THEN


                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4nr: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1)  THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                    tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      ((VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )       ) &
                                                                    +skilldepl* &
                                                                    ((VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)     )   ) &
                                                                    ) &
                                                                    +tempreal6

                                                  END IF


                                                END DO zloop_4nr
                                        END DO
                                    VUltemp(pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
END IF


                           !IF(runnumber<=3) WRITE(*,*) 'VU-----', VUltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VU-----', VUltemp(ppts,1, 1:MIN(zpts,10))

                            !~~~~~~~~~~~~~~~~~~~
                            !  5) New Employment value
                            !~~~~~~~~~~~~~~~~~~~~~~


                              DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                      tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                        (delta*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta)*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal6
                                                END DO
                                            END DO
                                        END DO
                                              VEltemp(pcnt, xcnt, zcnt)= prodl(pcnt, xcnt, zcnt) + bt*(1.0_8-dd)*tempreal6
                                    END DO
                                END DO
                            END DO

                            !IF(runnumber<=3) WRITE(*,*) 'VE-----', VEltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VE-----', VEltemp(ppts,1, 1:MIN(zpts,10))
                           ! IF(runnumber<=100 .AND. runnumber>3) WRITE(*,*) 'R,U,W', Reallocl(ppts), VUltemp(ppts,1, 1), VEltemp(ppts,1, 1)

!----------------------------------------------
!  TEST FOR CONVERGENCE
!---------------------------------------------

tempreal6=MAXVAL(ABS(VUltemp(:,:,:)-VUl(:,:,:))/ABS(REAL(VUltemp(:,:,:))))
tempreal5=MAXVAL(ABS(VEltemp(:,:,:)-VEl(:,:,:))/ABS(REAL(VEltemp(:,:,:))))

convdistance2=MAX(tempreal5,tempreal6)

IF(verbose_backw_ind==1 .AND. runnumber==2) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance
IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance2

!IF(verboseswitch==1) WRITE(*,*) 'runnumber=', runnumber, ' convcrit= ', convdistance
IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) WRITE(*,*) 'runnumber=', runnumber, ' convdistance= ', convdistance2


VUl=VUltemp
VEl=VEltemp

!IF(runnumber==2) WRITE(*,*) 'VEl(MAX(ppts/2, 1), 1, MAX(4*zpts)/5,1)', VEl(MAX((ppts/2), 1), 1, MAX((4*zpts)/5,1))

END DO convergence_loop

 IF(noreall_backwinduct_ind .ne. 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, zcnt), 0.0_8)+VUl(pcnt,xcnt, zcnt))) THEN
                                                        poliRl(pcnt, xcnt, zcnt)=0              ! 0 is reallocation
                                                ELSE
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        IF((reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, MAX(1,zcnt-1)), 0.0_8)+VUl(pcnt,xcnt, MAX(1,zcnt-1)))) &
                                                                .OR. zcnt==1) THEN
                                                            zrl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliRl(pcnt,xcnt,zpts)==0) zrl(pcnt,xcnt)=zpts+1
                                    END DO
                              END DO

ELSE IF(noreall_backwinduct_ind == 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        zrl(pcnt,xcnt)=1

                                       END DO
                                    END DO
                              END DO
    END IF


                            DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(VEl(pcnt,xcnt, zcnt)<VUl(pcnt,xcnt, zcnt)) THEN
                                                        poliDWl(pcnt, xcnt, zcnt)=0              ! 0 is separation
                                                ELSE
                                                        poliDWl(pcnt, xcnt, zcnt)=1              ! 1 is employment stay
                                                        IF(VEl(pcnt,xcnt, MAX(1,zcnt-1))<VUl(pcnt,xcnt, MAX(1,zcnt-1)) &
                                                                .OR. zcnt==1) THEN
                                                            zsl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliDWl(pcnt,xcnt,zpts)==0) zsl(pcnt,xcnt)=zpts+1
                                    END DO
                             END DO



!WRITE(*,*) 'reallocation cutoffs', zrl(:,1)
!WRITE(*,*) 'separation cutoffs', zsl(:,1)
!
!!---------------------------
!!  FINDING THE CUTOFFS
!!-------------------------------
!
!reservation_zr(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliR(pcnt,xcnt,1,1)==1)   reservation_zr(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliR(pcnt, xcnt, zcnt,1)==0 .AND. poliR(pcnt, xcnt, zcnt+1,1)==1) reservation_zr(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliR(pcnt, xcnt, zpts,1)==0) reservation_zr(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!
!reservation_zq(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliDW(pcnt,xcnt,1,1)==1)   reservation_zq(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliDW(pcnt, xcnt, zcnt,1)==0 .AND. poliDW(pcnt, xcnt, zcnt+1,1)==1) reservation_zq(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliDW(pcnt, xcnt, zpts,1)==0) reservation_zq(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!


!!_-_=_   WRITING THE CUTOFFS

!
!!--------------setup the increased precision
!
!
!tempcounter1=0          ! tempcounter1 is going to keep track of the highest possible reallocation
!tempcounter2=zpts       ! tempcounter2 is going to keep track of the lowest possible staying with a job
!
!DO pcnt=1, ppts
!    DO xcnt=1, xpts
!        DO zcnt=1, zpts
!            IF((poliDW(pcnt, xcnt, zcnt,2)==0 .OR. poliR(pcnt, xcnt, zcnt,2)==0) .AND. zcnt>tempcounter1) tempcounter1=zcnt
!            IF((poliDW(pcnt, xcnt, zcnt,2)==1 .OR. poliR(pcnt, xcnt, zcnt,2)==1) .AND. zcnt<tempcounter2) tempcounter2=zcnt
!        END DO
!    END DO
!END DO
!
!WRITE(*,*) 'HIGHEST POSSIBLE REALLOCATION OCCURS AT: ', tempcounter1
!WRITE(*,*) 'LOWEST POSSIBLE STAYING, OCCURS AT: ', tempcounter2
!
!IF (tempcounter2>2) zprecision_lb=zvector(MAX(tempcounter2-2, 1))
!IF (tempcounter1<zpts-1) zprecision_ub=zvector(MIN(tempcounter1+10, zpts))
!
!

!----------------reset current period value and policy functions
!IF(tworunnumber==1 .AND. tworun_switch==1) THEN
!    poliR(:,:,1)=0
!    poliDW(:,:,:, 1)=0
!    VEl(:,:,:, 1)=0.0_8
!    VUl(:,:,1)=0.0_8
!    jfl(:,:,1)=0.0_8
!    EV(:,:,1)=0.0_8
!    DmaxUl(:,:,1)=0.0_8
!    REALLOC(:,1)=0.0_8
!END IF

                           !------------------------------------------------------
                           ! 7) calculate WAGE PAYMENTS after convergence
                           !-----------------------------------------------------



        DO pcnt=1, ppts
            DO zcnt=1, zpts
                DO xcnt=1, xpts
                    tempreal6=0.0_8
                    ! first, calculate the continuation value
                    DO z2cnt=1, zpts
                        DO p2cnt=1, ppts
                           DO x2cnt=1, xpts
                                        tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                      ((  &
                                                                        (       (  delta+ (1-delta)*(1-poliDWl(p2cnt, x2cnt, z2cnt)) )  * VUl(p2cnt, x2cnt, z2cnt)       &
                                                                        ! +1(
                                                                       +      (1-delta)*poliDWl(p2cnt, x2cnt, z2cnt)*&                 ! 0 (open
                                                                        (    (1-eta)*VEl(p2cnt,x2cnt,z2cnt) + eta*VUl(p2cnt,x2cnt, z2cnt)       )   )  &               ! -1(
                                                                        )) &
                                                                       +tempreal6

!                                        tempreal6=ptransmatrix(pcnt, p2cnt)*ztransmatrix(zcnt, z2cnt)*&
!                                                                       ( (delta+ (1-delta)*(1-poliDW(p2cnt, xcnt, z2cnt, 2)))*VUl(p2cnt, z2cnt, 2) + (1-delta)*poliDW(p2cnt, xcnt,
!                                                                            z2cnt, 2)*&
!                                                                        (VEl(p2cnt, xcnt, z2cnt, 2)-k*( jfl(p2cnt, z2cnt, 2)**((1.0_8-eta)/eta) )) )  &
!                                                                       +tempreal6
                            END DO
                        END DO
                     END DO
                    wagel(pcnt, xcnt, zcnt)=(1-eta)*VEl(pcnt, xcnt, zcnt)+eta*VUl(pcnt, xcnt, zcnt) -bt*(1-dd)*tempreal6
                    !WAGE(pcnt, xcnt, zcnt,1)=VEl(pcnt, xcnt, zcnt)-k*( jfl(pcnt, zcnt)**((1.0_8-eta)/eta) ) -bt*tempreal6
               END DO
            END DO
        END DO



IF(verbose_backw_ind==1) WRITE(*,*) 'converged and calculated wages, distance2', convdistance2, 'iteration', runnumber
!PAUSE

  !===================================================================
!-----------------------------------------------------------------
!       WRITING VALUE/POLICY FUNCTIONS TO FILE
!----------------------------------------------------------------

!
!        !!-------------------- WRITE WAGES TO FILE
!        !IF (threadnol==1) THEN
!        !IF(verboseswitch==1) THEN
!        !    OPEN(UNIT=19, file="wage.csv", status="replace", form="formatted")
!        !    WRITE(UNIT=19, FMT='(5(A15,A1))') "agg prod", ",", "island prod", "," , "match prod",  "," , "w "
!        !    DO pcnt=1, ppts, (MAX(ppts/10,1))
!        !        DO xcnt=1, xpts, MAX((xpts/3),1)
!        !            DO zcnt=1, zpts, (zpts/10)
!        !               WRITE(19, *), pvector(pcnt), zvector(zcnt), xvector(xcnt), wage(pcnt, xcnt, zcnt,1)
!        !                WRITE(*,*), pvector(pcnt), zvector(zcnt), xvector(xcnt), wage(pcnt, xcnt, zcnt,1)
!        !             END DO
!        !        END DO
!        !    END DO
!        !    CLOSE(UNIT=19)
!        !
!        !!-------------------- WRITE EV, VE to file
!        !
!        !    OPEN(UNIT=19, file="ev_ve.txt", status="replace", form="formatted")
!        !
!        !               WRITE(19, FMT='(7(A3, A12)))', ADVANCE='NO'),"   ", "pvector,", "   ", "zvector,", "   ", "xvector,", "   ", "VEl(p, x, z)," , "   ", "pisswage "
!        !               WRITE(19, FMT='(4(A3, A12)))', ADVANCE='NO'),"   ", "wage lb,", "   ",  "const flow,", "   ", "wage(p,x,z)", "   ", "prod(p,x,z)"
!        !               WRITE(19, FMT='(3(A12, A3),2(A2,A3))'), "VUl(p,x,z),", "   ", "jfl(p,x,z)," , "   ", "value R(p)," , "   ", "D," , "   ", "R," ,"   "
!        !    DO pcnt=1, ppts, (MAX(ppts/10,1))
!        !        DO xcnt=1, xpts
!        !            DO zcnt=1, zpts
!        !               WRITE(19, FMT='(7(A3, F12.6)))', ADVANCE='NO'),"   ", pvector(pcnt), "   ", zvector(zcnt), "   ",xvector(xcnt), "   ",VEl(pcnt, xcnt, zcnt,1),"
!",(1-eta)*prod(pcnt, xcnt, zcnt)+eta*b+bt*(1-dd)*jfl(pcnt, xcnt, zcnt,1)**(1.0_8/eta)*k
!        !               WRITE(19, FMT='(4(A3, F12.6)))', ADVANCE='NO'),"   ", VEl(pcnt, xcnt, zcnt,1)*(1-bt*(1-dd)), "   ", (VEl(pcnt, xcnt, zcnt,1)-VUl(pcnt,xcnt,
!zcnt,1))*(1-bt*(1-dd)*(1-delta)+bt*(1-dd)*jfl(pcnt, xcnt, zcnt,1))+b, "   ", wage(pcnt, xcnt, zcnt,1), "   ",  prod(pcnt, xcnt, zcnt)
!        !               WRITE(19, FMT='(A3, 3(F12.6, A3),2(I2,A3))'), "   ", VUl(pcnt, xcnt, zcnt), "   ", jfl(pcnt, xcnt, zcnt,1), "   ", realloc(pcnt,1), "   ",poliDW(pcnt,
!xcnt, zcnt), "   ",poliR(pcnt, xcnt, zcnt,1),"   "
!        !!               WRITE(*,FMT='(5(F12.6, A3))'), pvector(pcnt), "   ",zvector(zcnt), "   ",xvector(xcnt), "   ",VEl(pcnt, xcnt, zcnt,1), "   ",ev(pcnt, zcnt,1), "   "
!        !             END DO
!        !        END DO
!        !    END DO
!        !    CLOSE(UNIT=19)
!        !END IF
!        !!---------------------- WRITE THE RESERVATION values for REALLOCATION, and for QUITS
!        !!PAUSE
!        !
!        !
!        !            ! for now, not much x heterogeneity
!        ! OPEN(unit=29, file='reservation.txt', status='replace', form='formatted')
!        ! WRITE(29, FMT='(A80)') '****reported numbers are highest z at which quit/reallocation takes place***'
!        ! WRITE(29, FMT='(A80)') '     ****xcnt=3, occupational human capital is accumulated***      '
!        !
!        ! WRITE(29, FMT='(A4, A1, 2X, A1, A11, A1, 2X)', ADVANCE='NO') 'pind', ',', ' ', 'pvector(p)', ','
!        !
!        ! ! note this is only for xpts=3
!        ! WRITE(29, FMT='(A4, A1, X, A1, A8, A1, X, A6, A1,X)', ADVANCE='NO') 'zRn1', ',' , ' ' , 'zR1', ',', 't_prod', ','
!        ! WRITE(29, FMT='(A4, A1, 2X, A1, A8, A1, 2X, A6, A1, X)', ADVANCE='NO') 'zQn1', ',' , ' ','zQ1', ',' ,'t_prod', ','
!        !
!        ! WRITE(29, FMT='(A4, A1, X, A1, A8, A1, X, A6, A1,X)', ADVANCE='NO') 'zRn2', ',' , ' ' , 'zR2', ',', 't_prod', ','
!        ! WRITE(29, FMT='(A4, A1, 2X, A1, A8, A1, 2X, A6, A1, X)', ADVANCE='NO') 'zQn2', ',' , ' ','zQ2', ',' ,'t_prod', ','
!        !
!        ! WRITE(29, FMT='(A4, A1, X, A1, A8, A1, X, A6, A1,X)', ADVANCE='NO') 'zRn3', ',' , ' ' , 'zR3', ',', 't_prod', ','
!        ! WRITE(29, FMT='(A4, A1, 2X, A1, A8, A1, 2X, A6, A1, X)') 'zQn3', ',' , ' ','zQ3', ',' ,'t_prod', ','
!        !
!        ! DO pcnt=1, ppts
!        !
!        !
!        !        ! write productivity ind, productivity
!        !   WRITE(29, FMT='(I4, A1)', ADVANCE='NO') pcnt, ','
!        !   WRITE(29, FMT='(2X, A1)', ADVANCE='NO') ' '
!        !   WRITE(29, FMT='(F11.6, A1, 2X)', ADVANCE='NO') pvector(pcnt), ','
!        !
!        !    DO xcnt=1, xpts
!        !
!        !     ! write reallocation cutoff ind, reallocation productivity. NOTE THAT WE TAKE xcnt=1 here, but really we should be talking about
!        !     ! average x-productivities
!        !
!        !
!        !    IF(poliR(pcnt,xcnt,1,1)==1) THEN
!        !                   WRITE(29, FMT='(I4, A1, X, A1, F8.6, A1, X, F6.3, A1,X)', ADVANCE='NO') 1, ',' , '<', zvector(1), ','  ,prod(pcnt,xcnt,1), ','
!        !    ELSE
!        !        DO zcnt=1, zpts-1
!        !            IF(poliR(pcnt, xcnt, zcnt,1)==0 .AND. poliR(pcnt, xcnt, zcnt+1,1)==1) THEN
!        !                   WRITE(29, FMT='(I4, A1, X, A1, F8.6, A1, X, F6.3, A1,X)', ADVANCE='NO') zcnt, ',' , ' ', zvector(zcnt), ','  ,prod(pcnt,xcnt,zcnt), ','
!        !            END IF
!        !        END DO
!        !        IF (poliR(pcnt, xcnt, zpts,1)==0) THEN
!        !                   WRITE(29, FMT='(I4, A1, X, A1, F8.6, A1, X, F6.3, A1, X)', ADVANCE='NO') zpts, ',' , '>', zvector(zpts), ',' ,prod(pcnt,xcnt,zpts), ','
!        !        END IF
!        !    END IF
!        ! ! write quit cutoff ind, quit cutoff productivity. AGAIN WE REPORT ONLY FOR XCNT==1, but really, we should be reporting it for all xcnts
!        ! ! if there is heterogeneity here
!        !
!        !    IF(poliDW(pcnt,xcnt,1, 1)==1) THEN
!        !                   WRITE(29, FMT='(I4, A1, 2X, A1, F8.6, A1, 2X, F6.3, A1, X)', ADVANCE='NO') 1, ',' , '<', zvector(1), ',', prod(pcnt,xcnt,1), ','
!        !    ELSE
!        !        DO zcnt=1, zpts-1
!        !            IF(poliDW(pcnt, xcnt, zcnt,1)==0 .AND. poliDW(pcnt, xcnt, zcnt+1,1)==1) THEN
!        !                   WRITE(29, FMT='(I4, A1, 2X, A1, F8.6, A1, 2X, F6.3, A1, X)', ADVANCE='NO') zcnt, ',' , ' ', zvector(zcnt), ',' , prod(pcnt,xcnt,zcnt), ','
!        !            END IF
!        !        END DO
!        !        IF (poliDW(pcnt, xcnt, zpts,1)==0) THEN
!        !                   WRITE(29, FMT='(I4, A1, 2X, A1, F11.6, A1, 2X, F6.3, A1, X)', ADVANCE='NO') zpts, ',' , '>', zvector(zpts), ',' ,prod(pcnt,xcnt,zpts), ','
!        !        END IF
!        !    END IF
!        !  END DO                        ! xcnt loop
!        !  WRITE(29, FMT='(A1)') ' '     ! move to the next line
!        ! END DO
!        !
!        ! CLOSE(29)
!        !   ! expected duration (taken as the inverse of the job finding rate here (somewhat waving our hands, because the actual empirical
!        !   ! one might be slightly more reverting to the mean productivity relation
!        !
!        !
!        !
!        ! OPEN(unit=30, file='duration.txt', status='replace', form='formatted')
!        !! for pcnt=2
!        !! for pcnt=ppts/2
!        !! for ppts-2
!        !
!        !WRITE(30, FMT='(10(A9,X))', ADVANCE='NO') 'zvector, ' ,  'jfl(mn,1,z),' , 'jfl(mu,1,z),'  , 'jfl(mx,1, z)',  'jfl(mn,2,z),' , 'jfl(mu,2,z),'  , 'jfl(mx,2, z)' ,
!'jfl(mn,3,z),' , 'jfl(mu,3,z),'  , 'jfl(mx,3, z)'
!        !WRITE(30, FMT='(A1)') ' '
!        !
!        !DO zcnt=1, zpts
!        !    DO xcnt=1, xpts
!        !        WRITE(30, FMT='(4(F9.6,A1))', ADVANCE='NO') zvector(zcnt), ',' , 1.0_8/jfl(1,xcnt, zcnt), ',' ,1.0_8/jfl(ppts/2, xcnt, zcnt,1), ',' , 1.0_8/jfl(ppts-1, xcnt,
!zcnt,1)
!        !    END DO
!        !    WRITE(30, FMT='(A1)') ' '
!        !END DO
!        !CLOSE(30)
!        !END IF !(threadnol==1



!======================================================================================================================
!======================================================================================================================
!======================================================================================================================
!  **HETEROGENEOUS DELTA BACKWARD INDUCTION**
!======================================================================================================================
!======================================================================================================================
!======================================================================================================================




    ELSE IF (heterogenous_delta_ind .eq. 1) THEN

    !**************************************************************
    !
    !I. Solve the model backwards
    !
    !**************************************************************

VUltemp=0.0_8
VEltemp=0.0_8
tempreal2=0.0_8
tempreal3=0.0_8
tempreal4=0.0_8
tempreal5=0.0_8
tempreal6=0.0_8

    !--------------------------------------------------------------
    !   LAST PERIOD + HOWARD's IMPROVEMENT ALGORITHM ITERATIONS
    !--------------------------------------------------------------

    IF(verboseswitch==1) WRITE(*,*) "Starting to solve the model"

!FOR EACH IS-realization, do step 1+2


    runnumber=0
    convdistance=10000

    IF (threadnol==1) THEN
    WRITE(10,*) 'beginning of convergence loop', runnumber, maxrunnumber, convdistance, convcrit
    END IF



convergence_loop_qtr2: &
    DO WHILE (convdistance>convcrit .AND. runnumber<MIN(500.0, maxrunnumber/2.0))

    runnumber=runnumber+1
    IF (verbose_backw_ind==1 .AND. runnumber==2) WRITE(*,*) 'entering quarterly convergence loop'




                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 1)  DETERMINING  TH(z,t), Dmax(t, V), DmaxUl(t)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                            ! Here we use the planner's problem:
                            !
                            !
                            !  FOC of  maximizing the surplus value of the application: k=eta*TH**eta-1 [EV(tmax)-V(zcnt,.tmax)]
                            !                   from  (1) k=q(TH) [EV - X], where X is the value applied to by the worker
                            !                    and   (2) Dmax(t,V)=max p(TH)[X-V]
                            !                                           SOLVE FOR X, and take a derivative wrt to TH to find the solution

                            ! 4a. UNEMPLOYED JOB FINDING

                                IF(nosearch_backwinduct_ind .NE. 1 ) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_v20: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      thetatemp=(   ( eta*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   )/k   )**( 1/(1-eta) )

                                                              IF(thetatemp>1) thetatemp=1.0_8

                                                      DmaxUl(pcnt, xcnt, zcnt)=pf(thetatemp)*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   -  k*thetatemp

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=pf(thetatemp)

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_v20
                                                    END IF
                                        END DO islandloop_qtr_v20
                                     END DO
                                END DO

                                ELSE IF (nosearch_backwinduct_ind==1) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr2_v21: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      DmaxUl(pcnt, xcnt, zcnt)=(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=1.0_8

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr2_v21
                                                    END IF
                                        END DO islandloop_qtr2_v21
                                     END DO
                                END DO
                                END IF

                                !IF(runnumber<=3) WRITE(*,*) DmaxUl(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 2)  REALLOCATION DECISION
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        IF(noreall_backwinduct_ind .ne. 1) THEN
                            IF (instantreall==0) THEN

                                DO pcnt=1, ppts
                                    tempreal6=0.0_8
                                            DO zcnt=1, zpts
                                                    ! this will be the continuation value after reallocating

                                                        tempreal6=znewpdfl(zcnt)*VUl(pcnt,1, zcnt) +tempreal6            ! figuring out E[REALLOC]
                                                                                                      ! reallocation choice    ! application choice                  ! NOTE, ZERO
                                                                                                      !TENURE IN OCCUPATION, SO NO HC
                                                                                                      ! reallocation choice    ! application choice


                                            END DO
                                            Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                END DO

                              ELSE IF (instantreall==1) THEN
                                    DO pcnt=1, ppts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                                        ! this will be the continuation value after reallocating

                                                            tempreal6=znewpdfl(zcnt)*(VUl(pcnt,1, zcnt)+DmaxUl(pcnt,1,zcnt)) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE,
                                                                                                          !ZERO TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice


                                                END DO
                                                Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                    END DO
                              END IF
        ELSE IF(noreall_backwinduct_ind == 1) THEN

                                Reallocl=-HUGE(1.0_8)

        END IF


!================================================
!  VALUE FUNCTION CALCULATION (implicit reallocation and separation decision)
!================================================





 IF(noreall_backwinduct_ind .ne. 1) THEN
                            P_DO: do pcnt=1, ppts
                            Z_DO:     do zcnt=1, zpts
                            X_DO:        do xcnt=1, xpts


                                    !-------
                                    ! VU CALCULATION
                                    !-------


                                    ! expected skill depreciation over 12 weeks
                                    !IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                    !    skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                    !    skilldepl12=MIN(1.0, skilldepl12)
                                    !ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                    !    skilldepl12=skilldepl
                                    !ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                    !    skilldepl12=MIN(1.0, 1.0-(1.0-skilldepl)**howard_no)
                                    !END IF
                                    IF (skilldepl .ne. 0.0_8) THEN

                                        IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                            skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                            skilldepl12=MAX(MIN(1.0, skilldepl12), 0.0_8)
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                            skilldepl12=skilldepl
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                            skilldepl12=MAX(MIN(1.0, 1.0-(1.0-skilldepl)**howard_no), 0.0)
                                        END IF
                                    ELSE IF (skilldepl .eq. 0.0_8) THEN
                                        skilldepl12=0.0_8
                                    END IF

                                        tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4qtr_v2: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 .AND. (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt==1 .AND. (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztranscdfl12(zcnt, z2cnt)* &
                                                      (REALLOCl(p2cnt))        +tempreal6
                                                    EXIT zloop_4qtr_v2

                                                  ELSE  IF(xpts>1 .AND. xcnt>1) THEN ! xcnt>1

                                                      IF((REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                                                          (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)))  ) THEN

                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl12)* &
                                                                      (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                                                                    +skilldepl12* &
                                                                    (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                                                                    ) &
                                                                    +tempreal6
                                                      ELSE IF  ((REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                                                          (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)  ))) THEN


                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztranscdfl12(zcnt, z2cnt)* (REALLOCl(p2cnt))        &
                                                                    +tempreal6
                                                            EXIT zloop_4qtr_v2
                                                        END IF
                                                END IF

                                                END DO zloop_4qtr_v2
                                        END DO


                                    !-------
                                    ! VE CALCULATION
                                    !-------
                                        tempreal5=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal5=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)*xtransmatrixl12(xcnt, x2cnt)*&
                                                                        (delta_vector(xcnt)*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta_vector(xcnt))*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal5
                                                END DO
                                            END DO
                                        END DO

                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VUltemp = b          *   (1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))                           + beta^12(1-dd)^12 (1-jf)^12 VU
                                    !                              =tempreal2
                                    !            y(p,x,z)   *   (1-btl12)/(1-beta(1-dd))-(1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                              =tempreal3           - tempreal2

                                    !-------
                                    ! HOWARD's IMPROVEMENT
                                    !-------



                                        IF (jfl(pcnt, xcnt, zcnt)>=0 .AND. jfl(pcnt, xcnt, zcnt)<1.0_8) THEN
                                            tempreal2=(1.0_8 - btl12*(1.0-jfl(pcnt, xcnt, zcnt))**howard_no)/(1.0_8-(1.0_8-jfl(pcnt, xcnt, zcnt))*bt*(1.0_8-dd))
                                            tempreal3=(1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                                ! tempreal6 = VU continuation value
                                                ! tempreal5 = VE continuation value
                                            VUltemp(pcnt, xcnt, zcnt)=b * tempreal2 &
                                                                        + (btl12*(1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no)*tempreal6 + &
                                                                      prodl(pcnt, xcnt, zcnt) * (tempreal3-tempreal2) &
                                                                        + btl12*(1.0-((1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no))*tempreal5
                                                ! note, we ignore the history where an unemployed worker finds a job and then loses it

                                         ! VU: compare to reallocation in week 1. Reallocate if profitable. (This has a imprecision of a week, but we ignore this as an
                                         !approximation)
                                            VUltemp(pcnt, xcnt, zcnt)=MAX(b + bt*(1.0_8-dd)*Reallocl(pcnt),VUltemp(pcnt, xcnt, zcnt))

                                        ELSE IF (jfl(pcnt, xcnt, zcnt)>=1.0) THEN
                                            VUltemp(pcnt, xcnt, zcnt)=b-prodl(pcnt,xcnt,zcnt)+ &
                                            ((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))*prodl(pcnt,xcnt,zcnt)+btl12*tempreal5
                                        ELSE
                                            VUltemp(pcnt, xcnt, zcnt)=b*((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))+btl12*tempreal6
                                        END IF


                                    ! VE, incorporate exogenous separations
                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VEltemp = y(p,x,z)          *   (1- ((1-delta)beta(1-dd))**12)/(1- (1-delta)beta(1-dd))       + beta^12(1-dd)^12 (1-delta)^12 VU
                                    !                                        =tempreal2
                                    !            b   *   (1-btl12)/(1-beta(1-dd))-(1- ((1-jf)beta(1-dd))**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                       =tempreal3           - tempreal2

                                                ! note we ignore the history where the worker loses a job and then finds it within the quarter. Here, this has more of a bite, as
                                                ! job finding rates are high enough to have a substantial amount of workers recover employment within the quarter. However,
                                                !exogenous
                                                ! separations themselves are still low, so overall the approximation is still helpful.
                                                ! to take this into account however, we use another rule simplification. Average duration is 1/jf, exogenous separation
                                                ! have on average 6 weeks. so survival in unemployment.
                                                ! tempreal4 captures the bouncing back into employment MIN(0.8,6/jf)

                                        !tempreal4= 1.0-MAX(MIN(0.8, 6.0/jfl(pcnt, xcnt, zcnt)),0.0_8)
                                        tempreal4=1.0_8
                                        tempreal2=  (1.0_8- btl12*((1.0_8-delta_vector(xcnt)*tempreal4)**howard_no))/(1.0- (1.0-delta_vector(xcnt)*tempreal4)*bt*(1.0-dd))
                                        tempreal3=  (1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                        VEltemp(pcnt, xcnt, zcnt) = prodl(pcnt, xcnt, zcnt)* tempreal2  &
                                                                        + (btl12*(1.0_8-delta*tempreal4)**howard_no)*tempreal5 + &
                                                                    b* (tempreal3-tempreal2)  &
                                                                        + btl12*(1.0-((1.0_8-delta*tempreal4)**howard_no))*tempreal6

                           !
                           !
                           !         ! VE, incorporate endogenous separation
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b + bt*(1.0_8-dd)*VUltemp(pcnt, xcnt, zcnt)),VEltemp(pcnt, xcnt, zcnt))
                           !
                           !         ! VE, compare to value of separation and reallocation. As this is just speeding up the convergence, we assume here that separations
                           !         !    lead to reallocation in the next period right away
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b*(1.0+bt*(1.0_8-dd)) + ((bt*(1.0_8-dd))**2)*Reallocl(pcnt)),VEltemp(pcnt, xcnt, zcnt))
                            END DO X_DO
                            END DO Z_DO
                            END DO P_DO

 ELSE IF(noreall_backwinduct_ind == 1) THEN

                        P_DOnr: do pcnt=1, ppts
                        Z_DOnr:     do zcnt=1, zpts
                        X_DOnr:          do xcnt=1, xpts


                                    !-------
                                    ! VU
                                    !-------


                                    ! expected skill depreciation over 12 weeks
                                    !IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                    !    skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                    !    skilldepl12=MIN(1.0, skilldepl12)
                                    !ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                    !    skilldepl12=skilldepl
                                    !ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                    !    skilldepl12=MIN(1.0, 1.0-(1.0-skilldepl)**howard_no)
                                    !END IF
                                    IF (skilldepl .ne. 0.0_8) THEN

                                        IF(jfl(pcnt,xcnt,zcnt)>0.0 .AND. jfl(pcnt,xcnt,zcnt)<1.0) THEN
                                            skilldepl12=skilldepl*(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))**howard_no))/(1.0-(((1.0-jfl(pcnt,xcnt,zcnt))*(1.0-skilldepl))))
                                            skilldepl12=MAX(MIN(1.0, skilldepl12), 0.0)
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)>=1.0) THEN
                                            skilldepl12=skilldepl
                                        ELSE IF (jfl(pcnt,xcnt,zcnt)<=0.0) THEN
                                            skilldepl12=MAX(MIN(1.0, 1.0-(1.0-skilldepl)**howard_no), 0.0_8)
                                        END IF
                                    ELSE IF (skilldepl .eq. 0.0_8) THEN
                                        skilldepl12=0.0_8
                                    END IF


                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4qtr2_v2: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 ) THEN
                                                      tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt>1) THEN   ! xcnt>1

                                                            tempreal6=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)* &
                                                                      ( &
                                                                    (1.0_8-skilldepl12)* &
                                                                      ((VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )       ) &
                                                                    +skilldepl12* &
                                                                    ((VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)     )   ) &
                                                                    ) &
                                                                    +tempreal6

                                                  END IF


                                                END DO zloop_4qtr2_v2
                                        END DO


                                    !-------
                                    ! VE
                                    !-------
                                        tempreal5=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal5=ptransmatrixl12(pcnt, p2cnt)*ztransmatrixl12(zcnt, z2cnt)*xtransmatrixl12(xcnt, x2cnt)*&
                                                                        (delta_vector(xcnt)*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta_vector(xcnt))*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal5
                                                END DO
                                            END DO
                                        END DO

                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VUltemp = b          *   (1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))                           + beta^12(1-dd)^12 (1-jf)^12 VU
                                    !                              =tempreal2
                                    !            y(p,x,z)   *   (1-btl12)/(1-beta(1-dd))-(1- (btl12*(1-jf)**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                              =tempreal3           - tempreal2


                                    !=============================
                                    ! Howard's improvement algorithm -type speed-up
                                    !=============================

                                        IF (jfl(pcnt, xcnt, zcnt)>=0 .AND. jfl(pcnt, xcnt, zcnt)<1.0_8) THEN
                                            tempreal2=(1.0_8 - btl12*(1.0-jfl(pcnt, xcnt, zcnt))**howard_no)/(1.0_8-(1.0_8-jfl(pcnt, xcnt, zcnt))*bt*(1.0_8-dd))
                                            tempreal3=(1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                                ! tempreal6 = VU continuation value
                                                ! tempreal5 = VE continuation value
                                            VUltemp(pcnt, xcnt, zcnt)=b * tempreal2 &
                                                                        + (btl12*(1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no)*tempreal6 + &
                                                                      prodl(pcnt, xcnt, zcnt) * (tempreal3-tempreal2) &
                                                                        + btl12*(1.0-((1.0_8-jfl(pcnt, xcnt, zcnt))**howard_no))*tempreal5
                                                ! note, we ignore the history where an unemployed worker finds a job and then loses it

                                         ! VU: compare to reallocation in week 1. Reallocate if profitable. (This has a imprecision of a week, but we ignore this as an
                                         !approximation)
                                            VUltemp(pcnt, xcnt, zcnt)=MAX(b + bt*(1.0_8-dd)*Reallocl(pcnt),VUltemp(pcnt, xcnt, zcnt))

                                        ELSE IF (jfl(pcnt, xcnt, zcnt)>=1.0) THEN
                                            VUltemp(pcnt, xcnt, zcnt)=b-prodl(pcnt,xcnt,zcnt)+ &
                                            ((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))*prodl(pcnt,xcnt,zcnt)+btl12*tempreal5
                                        ELSE
                                            VUltemp(pcnt, xcnt, zcnt)=b*((1.0-(bt*(1.0-dd))**(howard_no))/(1.0-(bt*(1.0-dd))))+btl12*tempreal6
                                        END IF


                                    ! VE, incorporate exogenous separations
                                    ! discount 12 weeks/1 qtr ahead. Assumption is that decision rules stay constant.
                                    !  VEltemp = y(p,x,z)          *   (1- ((1-delta)beta(1-dd))**12)/(1- (1-delta)beta(1-dd))       + beta^12(1-dd)^12 (1-delta)^12 VU
                                    !                                        =tempreal2
                                    !            b   *   (1-btl12)/(1-beta(1-dd))-(1- ((1-jf)beta(1-dd))**12)/(1- (1-jf)beta(1-dd))  + beta^12(1-dd)^12 (1-(1-jf)^12) VE
                                    !                       =tempreal3           - tempreal2

                                                ! note we ignore the history where the worker loses a job and then finds it within the quarter. Here, this has more of a bite, as
                                                ! job finding rates are high enough to have a substantial amount of workers recover employment within the quarter. However,
                                                !exogenous
                                                ! separations themselves are still low, so overall the approximation is still helpful.
                                                ! to take this into account however, we use another rule simplification. Average duration is 1/jf, exogenous separation
                                                ! have on average 6 weeks. so survival in unemployment.
                                                ! tempreal4 captures the bouncing back into employment MIN(0.8,6/jf)

                                        !tempreal4= 1.0-MAX(MIN(0.8, 6.0/jfl(pcnt, xcnt, zcnt)),0.0_8)
                                        tempreal4=1.0_8
                                        tempreal2=  (1.0_8- btl12*((1.0_8-delta_vector(xcnt)*tempreal4)**howard_no))/(1.0- (1.0-delta_vector(xcnt)*tempreal4)*bt*(1.0-dd))
                                        tempreal3=  (1.0_8 - btl12)/(1.0_8-bt*(1.0-dd))

                                        VEltemp(pcnt, xcnt, zcnt) = prodl(pcnt, xcnt, zcnt)* tempreal2  &
                                                                        + (btl12*(1.0_8-delta_vector(xcnt)*tempreal4)**howard_no)*tempreal5 + &
                                                                    b* (tempreal3-tempreal2)  &
                                                                        + btl12*(1.0-((1.0_8-delta_vector(xcnt)*tempreal4)**howard_no))*tempreal6

                           !
                           !
                           !         ! VE, incorporate endogenous separation
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b + bt*(1.0_8-dd)*VUltemp(pcnt, xcnt, zcnt)),VEltemp(pcnt, xcnt, zcnt))
                           !
                           !         ! VE, compare to value of separation and reallocation. As this is just speeding up the convergence, we assume here that separations
                           !         !    lead to reallocation in the next period right away
                           !             VEltemp(pcnt, xcnt, zcnt)=MAX((b*(1.0+bt*(1.0_8-dd)) + ((bt*(1.0_8-dd))**2)*Reallocl(pcnt)),VEltemp(pcnt, xcnt, zcnt))
                                    END DO X_DOnr
                                    END DO Z_DOnr
                            END DO P_DOnr





END IF

                           !IF(runnumber<=3) WRITE(*,*) 'VU-----', VUltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VU-----', VUltemp(ppts,1, 1:MIN(zpts,10))

                           !
                           ! !~~~~~~~~~~~~~~~~~
                           ! ! 4) New Unemployment value
                           ! !~~~~~~~~~~~~~~~~~
                           !
                           !
                           !
                           !DO pcnt=1, ppts
                           !     DO zcnt=1, zpts
                           !         DO xcnt=1, xpts
                           !         tempreal6=0.0_8
                           !             DO p2cnt=1, ppts
                           !                 zloop_4qtr: DO z2cnt=zpts,1,-1
                           !                       IF(xcnt==1 .AND. (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                           !                           tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                           !                           (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                           !                       !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                           !                       ELSE IF (xcnt==1 .AND. (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                           !                           tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* &
                           !                           (REALLOCl(p2cnt))        +tempreal6
                           !                         EXIT zloop_4qtr
                           !
                           !                       ELSE
                           !
                           !                           IF((REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                           !                               (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  ) THEN
                           !
                           !                                 tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                           !                                         ( &
                           !                                         (1.0_8-skilldepl)* &
                           !                                           (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                           !                                         +skilldepl* &
                           !                                         (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                           !                                         ) &
                           !                                         +tempreal6
                           !                           ELSE IF  ((REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                           !                               (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                           !
                           !
                           !                                 tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* (REALLOCl(p2cnt))        &
                           !                                         +tempreal6
                           !                                 EXIT zloop_4qtr
                           !                             END IF
                           !                     END IF
                           !
                           !                     END DO zloop_4qtr
                           !             END DO
                           !         VUltemp(pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                           !         END DO
                           !
                           !         END DO
                           !END DO
                           !
                           !
                           !!IF(runnumber<=3) WRITE(*,*) 'VU-----', VUltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !!IF(runnumber<=20) WRITE(*,*) 'VU-----', VUltemp(ppts,1, 1:MIN(zpts,10))
                           !
                           ! !~~~~~~~~~~~~~~~~~~~
                           ! !  5) New Employment value
                           ! !~~~~~~~~~~~~~~~~~~~~~~
                           !
                           !
                           !   DO pcnt=1, ppts
                           !     DO zcnt=1, zpts
                           !         DO xcnt=1, xpts
                           !           tempreal6=0.0_8
                           !             DO p2cnt=1, ppts
                           !                 DO z2cnt=1, zpts
                           !                     DO x2cnt=1, xpts
                           !                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                           !                                             (delta*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta)*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )
                           &
                           !                                            +tempreal6
                           !                     END DO
                           !                 END DO
                           !             END DO
                           !                   VEltemp(pcnt, xcnt, zcnt)= prodl(pcnt, xcnt, zcnt) + bt*(1.0_8-dd)*tempreal6
                           !         END DO
                           !     END DO
                           ! END DO

                            !IF(runnumber<=3) WRITE(*,*) 'VE-----', VEltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VE-----', VEltemp(ppts,1, 1:MIN(zpts,10))
                           ! IF(runnumber<=100 .AND. runnumber>3) WRITE(*,*) 'R,U,W', Reallocl(ppts), VUltemp(ppts,1, 1), VEltemp(ppts,1, 1)

        !---------------------------------------------
        !  TEST FOR CONVERGENCE
        !---------------------------------------------

        tempreal6=MAXVAL(ABS(VUltemp(:,:,:)-VUl(:,:,:))/(REAL(VUltemp(:,:,:))))
        tempreal5=MAXVAL(ABS(VEltemp(:,:,:)-VEl(:,:,:))/(REAL(VEltemp(:,:,:))))

        convdistance=MAX(tempreal5,tempreal6)

        !IF(runnumber==2) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance
        !IF((runnumber/1000)*1000-runnumber==0) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance

        !IF(verboseswitch==1) WRITE(*,*) 'runnumber=', runnumber, ' convcrit= ', convdistance
        !IF(verboseswitch==1 .AND. (runnumber/10)*10-runnumber==0) WRITE(*,*) 'thr', threadnol, 'howardrunno.=', runnumber, ' dist=', convdistance


        VUl=VUltemp
        VEl=VEltemp

        IF(verbose_backw_ind==1 .AND. runnumber<=2) THEN
            WRITE(*,*) 'VEl(MAX(ppts/2, 1), 1, MAX(4*zpts)/5,1)', VEl(MAX((ppts/2), 1), 1, MAX((4*zpts)/5,1))
            WRITE(*,*) 'thr', threadnol, 'howardrunno.=', runnumber, ' dist=', convdistance
        END IF
            END DO convergence_loop_qtr2


    !=======================================
    !  WRITE DOWN VALUE FUNCTIONS AND DECISION RULES
    !==============================================


IF(verbose_backw_ind==1) THEN



    !WRITE(*,*) 'thread reporting', i1
WRITE(x1,FMT='(I3.3)') threadnol !

filename='backw_ind_howard'//trim(x1)//'.csv'
OPEN(UNIT=38, file=filename, form='formatted', status='replace')

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'p_index', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'x_index', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_index', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'p_level', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'x_level', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_level', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'prod', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'wage', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'VE', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'VU', ','

write(38, FMT='(A20, A1)' , ADVANCE='NO') 'poliDW', ','
write(38, FMT='(A20, A1)' , ADVANCE='NO') 'poliR', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_binary', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_binary', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_func', ','
WRITE(38, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_func', ','

write(38, FMT='(A1)' ) ','



    !                                        epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
    !                                        epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim
    !

DO pcnt=1,ppts
DO xcnt=1, xpts
DO zcnt=1, zpts

write(38, FMT='(I4, A1)' , ADVANCE='NO') pcnt, ','
write(38, FMT='(I4, A1)' , ADVANCE='NO') xcnt, ','
write(38, FMT='(I4, A1)' , ADVANCE='NO') zcnt, ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') pvector_in(pcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') xcnt, ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zcnt, ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') prodl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') wagel(pcnt,xcnt,zcnt), ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') VEl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') VUl(pcnt, xcnt, zcnt), ','

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') poliDWl(pcnt, xcnt, zcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') poliRl(pcnt, xcnt,zcnt), ','

! EMPLOYMENT PER AGE
!! cutoffs
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliRl(pcnt,xcnt, 1)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliRl(pcnt,xcnt, zcnt-1)==0 .AND. poliRl(pcnt,xcnt, zcnt)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! separation cutoff
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliDWl(pcnt,xcnt, 1)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliDWl(pcnt,xcnt, zcnt-1)==0 .AND. poliDWl(pcnt,xcnt, zcnt)==1) THEN
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(38, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! cutoff z-values (as a function of p,x)
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zrl(pcnt, xcnt), ','
write(38, FMT='(F12.6, A1)' , ADVANCE='NO') zsl(pcnt, xcnt), ','
write(38, FMT='(A1)' ) ','

END DO  !zcnt
END DO  !xcnt
END DO  ! pcnt

CLOSE(38)



END IF


 !=================================================================================
 !-------------------
 !  WEEKLY LOOP
 !----------------------
 !==================================================================================



    VEltemp=0.0_8
    VUltemp=0.0_8
    convdistance2=10000



convergence_loop2: &
    DO WHILE (convdistance2>convcrit .AND. runnumber<maxrunnumber)

    runnumber=runnumber+1
    IF (verbose_backw_ind==1 .AND. convdistance2==10000) WRITE(*,*) 'WEEKLY runxno is', runnumber, '  on thread ', threadnol

   !IF(verboseswitch==1) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        !IF(verboseswitch==1 .AND. (runnumber/100)*100-runnumber==0) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        IF((verbose_backw_ind==1 .AND. (runnumber/100)*100-runnumber==0) ) WRITE(*,*) 'runxnumber is', runnumber, '  on thread ', threadnol, 'conv distance is:', convdistance
       !IF((runnumber/1000)*1000-runnumber==0 .AND. threadnol==1) WRITE(*,*) 'runnumber=', runnumber

        IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) THEN
            !CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
            !WRITE(10,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            !WRITE(*,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            call cpu_time ( t22 )
            write ( *, * ) '1000 iteration BI: Elapsed CPU time = ', t22 - t11
            !write ( 10, * ) 'Within Backw Induction Elapsed CPU time = ', t22 - t11
            call cpu_time(t11)
        END IF



            nos_if_v2: IF(nosearch_backwinduct_ind .NE. 1 ) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns2_v2: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      thetatemp=(   ( eta*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   )/k   )**( 1/(1-eta) )

                                                              IF(thetatemp>1) thetatemp=1.0_8

                                                      DmaxUl(pcnt, xcnt, zcnt)=pf(thetatemp)*(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))   -  k*thetatemp

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=pf(thetatemp)

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns2_v2
                                                    END IF
                                        END DO islandloop_qtr_ns2_v2
                                     END DO
                                END DO

          ELSE IF (nosearch_backwinduct_ind==1) THEN
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns_v2: DO zcnt=zpts, 1, -1

                                                    IF(VUl(pcnt, xcnt, zcnt)<VEl(pcnt, xcnt, zcnt)) THEN

                                                      DmaxUl(pcnt, xcnt, zcnt)=(VEl(pcnt, xcnt, zcnt)-VUl(pcnt, xcnt, zcnt))

                                                              IF(DmaxUl(pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jfl(pcnt, xcnt, zcnt)=1.0_8

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jfl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUl(pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns_v2
                                                    END IF
                                        END DO islandloop_qtr_ns_v2
                                     END DO
                                END DO
          END IF nos_if_v2

                                !IF(runnumber<=3) WRITE(*,*) DmaxUl(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 2)  REALLOCATION DECISION
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        noreal_if_v2: IF(noreall_backwinduct_ind .ne. 1) THEN
                            IF (instantreall==0) THEN

                                DO pcnt=1, ppts
                                    tempreal6=0.0_8
                                            DO zcnt=1, zpts
                                                    ! this will be the continuation value after reallocating

                                                        tempreal6=znewpdfl(zcnt)*VUl(pcnt,1, zcnt) +tempreal6            ! figuring out E[REALLOC]
                                                                                                      ! reallocation choice    ! application choice                  ! NOTE, ZERO
                                                                                                      !TENURE IN OCCUPATION, SO NO HC
                                                                                                      ! reallocation choice    ! application choice


                                            END DO
                                            Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                END DO

                              ELSE IF (instantreall==1) THEN
                                    DO pcnt=1, ppts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                                        ! this will be the continuation value after reallocating

                                                            tempreal6=znewpdfl(zcnt)*(VUl(pcnt,1, zcnt)+DmaxUl(pcnt,1,zcnt)) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE,
                                                                                                          !ZERO TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice


                                                END DO
                                                Reallocl(pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                    END DO
                              END IF
        ELSE IF(noreall_backwinduct_ind == 1) THEN

                                Reallocl=-HUGE(1.0_8)

        END IF noreal_if_v2


                            !~~~~~~~~~~~~~~~~~
                            ! 4) New Unemployment value
                            !~~~~~~~~~~~~~~~~~

IF(noreall_backwinduct_ind .ne. 1) THEN


                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4_v2: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1 .AND. (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt==1 .AND. (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* &
                                                      (REALLOCl(p2cnt))        +tempreal6
                                                    EXIT zloop_4_v2

                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                      IF((REALLOCl(p2cnt)<(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                                                          (REALLOCl(p2cnt)<(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)))  ) THEN

                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                                                                    +skilldepl* &
                                                                    (MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                                                                    ) &
                                                                    +tempreal6
                                                      ELSE IF  ((REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                                                          (REALLOCl(p2cnt)>=(VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)  ))) THEN


                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* (REALLOCl(p2cnt))        &
                                                                    +tempreal6
                                                            EXIT zloop_4_v2
                                                        END IF
                                                END IF

                                                END DO zloop_4_v2
                                        END DO
                                    VUltemp(pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
ELSE IF(noreall_backwinduct_ind == 1) THEN


                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4nr_v2: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1)  THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                    tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      ((VUl(p2cnt, xcnt, z2cnt) +MAX(DmaxUl(p2cnt, xcnt, z2cnt), 0.0_8) )       ) &
                                                                    +skilldepl* &
                                                                    ((VUl(p2cnt, xcnt-1, z2cnt) +MAX(DmaxUl(p2cnt, xcnt-1, z2cnt), 0.0_8)     )   ) &
                                                                    ) &
                                                                    +tempreal6

                                                  END IF


                                                END DO zloop_4nr_v2
                                        END DO
                                    VUltemp(pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
END IF


                           !IF(runnumber<=3) WRITE(*,*) 'VU-----', VUltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VU-----', VUltemp(ppts,1, 1:MIN(zpts,10))

                            !~~~~~~~~~~~~~~~~~~~
                            !  5) New Employment value
                            !~~~~~~~~~~~~~~~~~~~~~~


                              DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                      tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                        (delta_vector(xcnt)*VUl(p2cnt, x2cnt, z2cnt)+ (1.0_8-delta_vector(xcnt))*MAX(VUl(p2cnt, x2cnt, z2cnt),(VEl(p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal6
                                                END DO
                                            END DO
                                        END DO
                                              VEltemp(pcnt, xcnt, zcnt)= prodl(pcnt, xcnt, zcnt) + bt*(1.0_8-dd)*tempreal6
                                    END DO
                                END DO
                            END DO

                            !IF(runnumber<=3) WRITE(*,*) 'VE-----', VEltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VE-----', VEltemp(ppts,1, 1:MIN(zpts,10))
                           ! IF(runnumber<=100 .AND. runnumber>3) WRITE(*,*) 'R,U,W', Reallocl(ppts), VUltemp(ppts,1, 1), VEltemp(ppts,1, 1)

!----------------------------------------------
!  TEST FOR CONVERGENCE
!---------------------------------------------

tempreal6=MAXVAL(ABS(VUltemp(:,:,:)-VUl(:,:,:))/ABS(REAL(VUltemp(:,:,:))))
tempreal5=MAXVAL(ABS(VEltemp(:,:,:)-VEl(:,:,:))/ABS(REAL(VEltemp(:,:,:))))

convdistance2=MAX(tempreal5,tempreal6)

IF(verbose_backw_ind==1 .AND. runnumber==2) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance
IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) WRITE(10,*) 'runnumber=', runnumber, ' convdistance= ', convdistance

!IF(verboseswitch==1) WRITE(*,*) 'runnumber=', runnumber, ' convcrit= ', convdistance
IF(verbose_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber==0) WRITE(*,*) 'runnumber=', runnumber, ' convdistance= ', convdistance


VUl=VUltemp
VEl=VEltemp

!IF(runnumber==2) WRITE(*,*) 'VEl(MAX(ppts/2, 1), 1, MAX(4*zpts)/5,1)', VEl(MAX((ppts/2), 1), 1, MAX((4*zpts)/5,1))

END DO convergence_loop2

 IF(noreall_backwinduct_ind .ne. 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, zcnt), 0.0_8)+VUl(pcnt,xcnt, zcnt))) THEN
                                                        poliRl(pcnt, xcnt, zcnt)=0              ! 0 is reallocation
                                                ELSE
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        IF((reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, MAX(1,zcnt-1)), 0.0_8)+VUl(pcnt,xcnt, MAX(1,zcnt-1)))) &
                                                                .OR. zcnt==1) THEN
                                                            zrl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliRl(pcnt,xcnt,zpts)==0) zrl(pcnt,xcnt)=zpts+1
                                    END DO
                              END DO

ELSE IF(noreall_backwinduct_ind == 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        zrl(pcnt,xcnt)=1

                                       END DO
                                    END DO
                              END DO
    END IF


                            DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(VEl(pcnt,xcnt, zcnt)<VUl(pcnt,xcnt, zcnt)) THEN
                                                        poliDWl(pcnt, xcnt, zcnt)=0              ! 0 is separation
                                                ELSE
                                                        poliDWl(pcnt, xcnt, zcnt)=1              ! 1 is employment stay
                                                        IF(VEl(pcnt,xcnt, MAX(1,zcnt-1))<VUl(pcnt,xcnt, MAX(1,zcnt-1)) &
                                                                .OR. zcnt==1) THEN
                                                            zsl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliDWl(pcnt,xcnt,zpts)==0) zsl(pcnt,xcnt)=zpts+1
                                    END DO
                             END DO



!WRITE(*,*) 'reallocation cutoffs', zrl(:,1)
!WRITE(*,*) 'separation cutoffs', zsl(:,1)
!
!!---------------------------
!!  FINDING THE CUTOFFS
!!-------------------------------
!
!reservation_zr(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliR(pcnt,xcnt,1,1)==1)   reservation_zr(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliR(pcnt, xcnt, zcnt,1)==0 .AND. poliR(pcnt, xcnt, zcnt+1,1)==1) reservation_zr(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliR(pcnt, xcnt, zpts,1)==0) reservation_zr(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!
!reservation_zq(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliDW(pcnt,xcnt,1,1)==1)   reservation_zq(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliDW(pcnt, xcnt, zcnt,1)==0 .AND. poliDW(pcnt, xcnt, zcnt+1,1)==1) reservation_zq(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliDW(pcnt, xcnt, zpts,1)==0) reservation_zq(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!


!!_-_=_   WRITING THE CUTOFFS

!
!!--------------setup the increased precision
!
!
!tempcounter1=0          ! tempcounter1 is going to keep track of the highest possible reallocation
!tempcounter2=zpts       ! tempcounter2 is going to keep track of the lowest possible staying with a job
!
!DO pcnt=1, ppts
!    DO xcnt=1, xpts
!        DO zcnt=1, zpts
!            IF((poliDW(pcnt, xcnt, zcnt,2)==0 .OR. poliR(pcnt, xcnt, zcnt,2)==0) .AND. zcnt>tempcounter1) tempcounter1=zcnt
!            IF((poliDW(pcnt, xcnt, zcnt,2)==1 .OR. poliR(pcnt, xcnt, zcnt,2)==1) .AND. zcnt<tempcounter2) tempcounter2=zcnt
!        END DO
!    END DO
!END DO
!
!WRITE(*,*) 'HIGHEST POSSIBLE REALLOCATION OCCURS AT: ', tempcounter1
!WRITE(*,*) 'LOWEST POSSIBLE STAYING, OCCURS AT: ', tempcounter2
!
!IF (tempcounter2>2) zprecision_lb=zvector(MAX(tempcounter2-2, 1))
!IF (tempcounter1<zpts-1) zprecision_ub=zvector(MIN(tempcounter1+10, zpts))
!
!

!----------------reset current period value and policy functions
!IF(tworunnumber==1 .AND. tworun_switch==1) THEN
!    poliR(:,:,1)=0
!    poliDW(:,:,:, 1)=0
!    VEl(:,:,:, 1)=0.0_8
!    VUl(:,:,1)=0.0_8
!    jfl(:,:,1)=0.0_8
!    EV(:,:,1)=0.0_8
!    DmaxUl(:,:,1)=0.0_8
!    REALLOC(:,1)=0.0_8
!END IF

                           !------------------------------------------------------
                           ! 7) calculate WAGE PAYMENTS after convergence
                           !-----------------------------------------------------



        DO pcnt=1, ppts
            DO zcnt=1, zpts
                DO xcnt=1, xpts
                    tempreal6=0.0_8
                    ! first, calculate the continuation value
                    DO z2cnt=1, zpts
                        DO p2cnt=1, ppts
                           DO x2cnt=1, xpts
                                        tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                      ((  &
                                                                        (       (  delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDWl(p2cnt, x2cnt, z2cnt)) )  * VUl(p2cnt, x2cnt, z2cnt)       &
                                                                        ! +1(
                                                                       +      (1-delta_vector(xcnt))*poliDWl(p2cnt, x2cnt, z2cnt)*&                 ! 0 (open
                                                                        (    (1-eta)*VEl(p2cnt,x2cnt,z2cnt) + eta*VUl(p2cnt,x2cnt, z2cnt)       )   )  &               ! -1(
                                                                        )) &
                                                                       +tempreal6

!                                        tempreal6=ptransmatrix(pcnt, p2cnt)*ztransmatrix(zcnt, z2cnt)*&
!                                                                       ( (delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDW(p2cnt, xcnt, z2cnt, 2)))*VUl(p2cnt, z2cnt, 2) + (1-delta_vector(xcnt))*poliDW(p2cnt, xcnt,
!                                                                            z2cnt, 2)*&
!                                                                        (VEl(p2cnt, xcnt, z2cnt, 2)-k*( jfl(p2cnt, z2cnt, 2)**((1.0_8-eta)/eta) )) )  &
!                                                                       +tempreal6
                            END DO
                        END DO
                     END DO
                    wagel(pcnt, xcnt, zcnt)=(1.0_8-eta)*VEl(pcnt, xcnt, zcnt)+eta*VUl(pcnt, xcnt, zcnt) -bt*(1-dd)*tempreal6
                    !WAGE(pcnt, xcnt, zcnt,1)=VEl(pcnt, xcnt, zcnt)-k*( jfl(pcnt, zcnt)**((1.0_8-eta)/eta) ) -bt*tempreal6
               END DO
            END DO
        END DO



IF(verbose_backw_ind==1) WRITE(*,*) 'converged and calculated wages, distance2', convdistance2, 'iteration', runnumber
!P





    END IF heterogenous_delta_if

!!========================================================================================================================
    !!================================================
    !!  NET MOBILITY BACKWARDS INDUCTION
    !!==================================================
!=========================================================================================================================


    !! A. CONSTRUCT INITIAL GUESS FOR EACH OCCUPATION

!    But in the setup we want to use the converged value function without occupation heterogeneity. Then we want to scale it a bit (auxiliary factor that can be taken along), so that our initial guess for this backwards induction is good.
!-	Employed Value function guessing. A parameter to shift weight around.
!o	Calculate VB=b/(1-beta(1-dd))
!o	NEW VE(p,o,x,z)=p(o,x,z)/p(x,z) (VE(p,x,z)-VB) +  VB
!o	OLD: VE(p,o,x,z) =((Occprod(p,z,o,x)-guess_par*benefit)/(pzx)  + (guess_par*Benefit)/pzx) VE_no_occ(p,z,x)
!-	Unemployed Value function guessing.
!o	VU(p,o,x,z)=p(o,x,z)/p(x,z) (VU(p,x,z)-VB) +  VB
!o	This guess is like there is no reallocation across 4 categories, only within.

    VB=b/(1.0-bt*(1.0_8-dd))

    !! VE, VU starting point
    DO occcnt=1, occpts
        DO pcnt=1, ppts
            DO xcnt=1, xpts
                DO zcnt=1, zpts
                    VElo(occcnt, pcnt, xcnt, zcnt)=(0.5*(prodlo(occcnt, pcnt, xcnt, zcnt)/prodl(pcnt, xcnt, zcnt))+0.5)*(VEl(pcnt, xcnt,zcnt)-VB)+ VB
                    VUlo(occcnt, pcnt, xcnt, zcnt)=(0.5*(prodlo(occcnt, pcnt, xcnt, zcnt)/prodl(pcnt, xcnt, zcnt))+0.5)*(prodlo(occcnt, pcnt, xcnt, zcnt)/prodl(pcnt, xcnt, zcnt))*(VUl(pcnt, xcnt,zcnt)-VB)+ VB
                    
                END DO
            END DO
        END DO
    END DO

    DO occcnt=1, occpts
        DO pcnt=1, ppts
            Realloclo(occcnt, pcnt)=Reallocl(pcnt)
        END DO
    END DO 

    !! A2. OPEN FILE TO WRITE TO 
    IF(verbose_during_nm_backw_ind_io==1) THEN
        !WRITE(*,*) 'thread reporting', i1
        WRITE(x1,FMT='(I3.3)') threadnol !
        filename='dur_nm_backw_ind'//trim(x1)//'.csv'
        OPEN(UNIT=40, file=filename, form='formatted', status='replace')
        ! iterationh
                                        WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'runno', ',x,'
                                        WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'pcnt', ',x,'
                                        WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'occno', ',x,'
                                        WRITE(40, FMT='((A10,A1))', ADVANCE='no') 'ERbase', ','

                                        ! continuation per destination occupation
                                        ! expectation of reallocation without shading search direction twoards certain occuaptions
                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'occs_elas', ',' 
                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par11', ',' 
                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par12', ',' 
                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par13', ',' 
                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par14', ',' 
                                        DO xcnt=1, 4
                                            WRITE(40, FMT='((A6,I1,A3,A1))', ADVANCE='no') 'Rdest_',xcnt,'   ', ','
                                        END DO
                                        WRITE(40, FMT='((A10,A1))', ADVANCE='no') 'ERbase2', ','
                                                        
                                                       
                                        
                                        
                                                        WRITE(40, FMT='(A20, A1)', ADVANCE='no') 'lambdatemp', ',' 
                                                        WRITE(40, FMT='(A40, A1)', ADVANCE='no') 'test1_partofsummation_in_lambda', ',' 
                                                        WRITE(40, FMT='(A40, A1)', ADVANCE='no') 'test2_partofsum_in_lambda_to_power', ',' 
                                                        WRITE(40, FMT='(A40, A1)', ADVANCE='no') 'power_component_in_sum_lambda', ',' 
                                                        WRITE(40, FMT='(A40, A1)', ADVANCE='no') 'part_power_component', ',' 
                                                        WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'test5', ',' 

                                        DO xcnt=1, occpts
                                            ! opt. search dir temp
                                            WRITE(40, FMT='((A8,I1,A1, A1))', ADVANCE='no') 'sdirtemp',xcnt, ' ', ','
                                        END DO
                                        DO xcnt=1, occpts
                                            ! opt. search dir final
                                            WRITE(40, FMT='((A9,I1, A1))', ADVANCE='no') 'sdirfinal',xcnt, ','
                                            ! continuation per destination occupation
                                        END DO 
                                        DO occcnt=1, occpts
                                            ! value of reallocating per superocc
                                            WRITE(40, FMT='((A9,I1, A1))', ADVANCE='no') 'realloclo_temp',occcnt, ','
                                        END DO 
                                        DO occcnt=1, occpts
                                        DO xcnt=1, occpts
                                            ! value of reallocating per superocc
                                            WRITE(40, FMT='((A6,I1,A1,I1, A1))', ADVANCE='no') 'sdircum',occcnt,'_',xcnt, ','
                                        END DO
                                        END DO
                                        WRITE(40, FMT='(A1)') ',' 
                                    
    END IF 
    
    
    
    
    
    !! B. DO THE BACKWARDS INDUCTION




    VElotemp=0.0_8
    VUlotemp=0.0_8
    convdistance3=10000
    runnumber_nm=1


convergence_loop3: &
    DO WHILE ((convdistance3>convcrit .AND. runnumber_nm<maxrunnumber_nm) .OR. runnumber_nm<480)

    runnumber_nm=runnumber_nm+1
    IF (verbose_nm_backw_ind==1 .AND. convdistance3==10000) WRITE(*,*) 'WEEKLY runxno is', runnumber, '  on thread ', threadnol

   !IF(verboseswitch==1) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        !IF(verboseswitch==1 .AND. (runnumber/100)*100-runnumber==0) WRITE(*,*) 'runxnumber is', runnumber, '  conv distance is:', convdistance
        IF(verbose_nm_backw_ind==1 .AND. (runnumber_nm/100)*100-runnumber_nm==0 .OR. &
            (valuefct_accel==1 .AND. runnumber_nm<40)) WRITE(*,*) 'runnumber_nm=', runnumber_nm, '  on thread ', threadnol, 'conv dist3 is:', convdistance3
       !IF((runnumber/1000)*1000-runnumber==0 .AND. threadnol==1) WRITE(*,*) 'runnumber=', runnumber

        IF(verbose_nm_backw_ind==1 .AND. (runnumber/1000)*1000-runnumber_nm==0) THEN
            !CALL DATE_AND_TIME(time_string(1), time_string(2), time_string(3))
            !WRITE(10,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            !WRITE(*,*) 'date=', time_string(1), 'finished time=', time_string(2), 'starting time=', time_string_begin(2)
            call cpu_time ( t22 )
            write ( *, * ) '1000 iteration BI: Elapsed CPU time = ', t22 - t11
            !IF(verbose_progress_bwind==1) write ( 10, * ) 'Within Backw Induction Elapsed CPU time = ', t22 - t11
            call cpu_time(t11)
        END IF



            nos_if_v3: &
                IF(nosearch_backwinduct_ind .NE. 1 ) THEN
                            DO occcnt=1, occpts
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns2_v3: &
                                            DO zcnt=zpts, 1, -1

                                                    IF(VUlo(occcnt, pcnt, xcnt, zcnt)<VElo(occcnt, pcnt, xcnt, zcnt)) THEN

                                                      thetatemp=(   ( eta*(VElo(occcnt, pcnt, xcnt, zcnt)-VUlo(occcnt, pcnt, xcnt, zcnt))   )/k   )**( 1/(1-eta) )

                                                              IF(thetatemp>1) thetatemp=1.0_8

                                                      DmaxUlo(occcnt, pcnt, xcnt, zcnt)=pf(thetatemp)*(VElo(occcnt, pcnt, xcnt, zcnt)-VUlo(occcnt,pcnt, xcnt, zcnt))   -  k*thetatemp

                                                              IF(DmaxUlo(occcnt, pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jflo(occcnt, pcnt, xcnt, zcnt)=pf(thetatemp)

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jflo(occcnt, pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DmaxUlo(occcnt, pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns2_v3
                                                    END IF
                                            END DO islandloop_qtr_ns2_v3
                                     END DO
                                END DO
                                END DO !occcnt

    ELSE IF (nosearch_backwinduct_ind==1) THEN
                        DO occcnt=1, occpts
                                DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                        islandloop_qtr_ns_v3: &
                                            DO zcnt=zpts, 1, -1

                                                    IF(VUlo(occcnt, pcnt, xcnt, zcnt)<VElo(occcnt, pcnt, xcnt, zcnt)) THEN

                                                      DMaxUlo(occcnt, pcnt, xcnt, zcnt)=(VElo(occcnt, pcnt, xcnt, zcnt)-VUlo(occcnt, pcnt, xcnt, zcnt))

                                                              IF(DMaxUlo(occcnt, pcnt, xcnt, zcnt)<0.0) THEN
                                                                  WRITE(*,*) 'ERROR: surplus cannot be lower than 0'
                                                                END IF


                                                      jflo(occcnt, pcnt, xcnt, zcnt)=1.0_8

                                                    ELSE
                                                      !TH_U(ycnt, expertemp, tmax)=0.0_8
                                                      jflo(occcnt, pcnt, xcnt, 1:zcnt)=0.0_8
                                                      DMaxUlo(occcnt, pcnt, xcnt, 1:zcnt)=0.0_8
                                                      EXIT islandloop_qtr_ns_v3
                                                    END IF
                                        END DO islandloop_qtr_ns_v3
                                     END DO
                        END DO
                        END DO ! occcnt
          END IF nos_if_v3

                                !IF(runnumber<=3) WRITE(*,*) DMaxUlo(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            ! 2)  REALLOCATION DECISION
                            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    noreal_if_v3:&
    IF(noreall_backwinduct_ind .ne. 1) THEN
    IF (instantreall==0) THEN

                    
                    !! NO DRAW MEANS REALLOCATION CONTINUATION VALUE TOMORROW        
                    

                reall_aggstate_do:&
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!
                DO pcnt=1, ppts
                    
                                    DO occcnt=1, occpts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                  
                                                            tempreal6=znewpdfl(zcnt)*VUlo(occcnt, pcnt,1, zcnt) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE, ZERO
                                                                                                          ! TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice
                
                
                                                END DO
                                  
                                                temp_R_destocclo(occcnt, pcnt)=tempreal6 
                
                                    END DO !occcnt
                                    
                                  
                
                                            ! PER SOURCE OCCUPATION
                        find_lambdastar_perocc_do:&
                        DO occcnt=1, occpts
                                  
                                  
                                    !! HOW TO MODEL THE CONTINUATION VALUE WHEN DRAWING OCCUPATIONS TO REALLOCATE TO
                                    ! CONTINUE TRY TO REALLOCATE NEXT TIME AROUND
                                    !note: realloclo includes c cost (subtracted)
                                    temp_ERo_baseline(occcnt, pcnt)=0.0_8
                                    DO p2cnt=1, ppts
                                        temp_ERo_baseline(occcnt, pcnt)=temp_ERo_baseline(occcnt, pcnt)+ptransmatrixl(pcnt, p2cnt)*realloclo(occcnt, p2cnt)
                                    END DO 
                                    temp_ERo_baseline(occcnt, pcnt)=b+ bt*(1.0-dd)*temp_ERo_baseline(occcnt, pcnt)
                                    tempnorm=temp_ERo_baseline(occcnt, pcnt)
                        
                        
                                    IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1 .AND. occcnt==1) THEN
                                        ! iterationh
                                        WRITE(40, FMT='(I5, A3)', ADVANCE='no') runnumber_nm, ',x,' 
                                        WRITE(40, FMT='(I5, A3)', ADVANCE='no') pcnt, ',x,' 
                                        WRITE(40, FMT='(I5, A3)', ADVANCE='no') occcnt, ',x,' 
                                        WRITE(40, FMT='((F10.4,A1))', ADVANCE='no') temp_ERo_baseline(occcnt, pcnt), ','
                                        !WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'runno', ',x,'
                                        !WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'pcnt', ',x,'
                                        !WRITE(40, FMT='(A5, A3)', ADVANCE='no') 'occno', ',x,'
                                        !WRITE(40, FMT='((A10,A1))', ADVANCE='no') 'ERbase', ','

                                        ! continuation per destination occupation
                                        ! DO xcnt=1, 4
                                        !    WRITE(40, FMT='((F10.4,A1))', ADVANCE='no') temp_R_destocclo(xcnt, pcnt), ','
                                        !END DO
                                        ! expectation of reallocation without shading search direction twoards certain occuaptions
                                        !WRITE(40, FMT='((F10.4,A1))', ADVANCE='no') temp_ERo_baseline(pcnt), ','
                                    END IF
                        
                                                    
                                    
                                                    lambdasol=0.0_8
                                                    
                                                    DO xcnt=1, occpts
                                                        IF(temp_R_destocclo(xcnt,pcnt)-temp_ERo_baseline(occcnt,pcnt)>0.0) THEN
                                                            
                                                            tempreal=(&
                                                                ((searchdir_parsl(occcnt, xcnt))**(1.0-occsearch_elasl)) * &
                                                                ((temp_R_destocclo(xcnt,pcnt)-temp_ERo_baseline(occcnt,pcnt))/temp_ERo_baseline(occcnt,pcnt)) &
                                                                )
                                                            tempreal2=(REAL(1.0)/(1.0-occsearch_elasl))
                                                            !! lambdasol is ( lambda/(nu*ERobaseline) )**(1/(1-nu))
                                                            lambdasol=lambdasol + (tempreal)**tempreal2
                                                        END IF 
                                                    END DO 
                                                    !! lambdasol is ( lambda/(nu*ERobaseline) )**(1/(1-nu))
                                                    lambdasol_opvector(occcnt, pcnt)=lambdasol
                                                    
                                                IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1) THEN
                                                        ! lambdasol temporary
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no') occsearch_elasl, ',' 
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no') searchdir_parsl(occcnt, 1), ',' 
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no') searchdir_parsl(occcnt, 2), ',' 
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no') searchdir_parsl(occcnt, 3), ',' 
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no') searchdir_parsl(occcnt, 4), ',' 
                                                        
                                        ! continuation per destination occupation
                                        ! expectation of reallocation without shading search direction twoards certain occuaptions
                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'occs_elas', ',' 
                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par11', ',' 
                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par12', ',' 
                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par13', ',' 
                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'sdir_par14', ',' 
                                        !
                                                        
                                                        
                                                        DO xcnt=1, 4
                                                                WRITE(40, FMT='((F10.4,A1))', ADVANCE='no') temp_R_destocclo(xcnt, pcnt), ','
                                                        END DO
                                                        WRITE(40, FMT='((F10.4,A1))', ADVANCE='no') temp_ERo_baseline(occcnt,pcnt), ','
                                                        
                                                        
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'lambdatemp', ',' 
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'test1_partofsummation_in_lambda', ',' 
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'test2_partofsum_in_lambda_to_power', ',' 
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'power_component_in_sum_lambda', ',' 
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'part_power_component', ',' 
                                                        !WRITE(40, FMT='(A10, A1)', ADVANCE='no') 'test5', ',' 
                                                        WRITE(40, FMT='(E10.5, A1)', ADVANCE='no') lambdasol, ','
                                                        xcnt=1
                                                        tempreal=(((searchdir_parsl(occcnt, xcnt))**(1.0-occsearch_elasl))* &
                                                                ((temp_R_destocclo(xcnt,pcnt)-temp_ERo_baseline(occcnt,pcnt))/temp_ERo_baseline(occcnt,pcnt)) &
                                                                )
                                                        WRITE(40, FMT='(E10.5, A1)', ADVANCE='no')  tempreal, ',' 
                                                        WRITE(40, FMT='(E10.5, A1)', ADVANCE='no')  tempreal**(REAL(((1.0_8)/(1.0_8-occsearch_elasl)))), ',' 
                                                        WRITE(40, FMT='(E10.5, A1)', ADVANCE='no')  (REAL(1.0)/(1.0-occsearch_elasl)), ',' 
                                                        WRITE(40, FMT='(E10.5, A1)', ADVANCE='no')  (1.0-occsearch_elasl), ',' 
                                                        WRITE(40, FMT='(F10.5, A1)', ADVANCE='no')  occsearch_elasl, ',' 
                                                        ! continuation per destination occupation
                                                        
                                                        DO xcnt=1, 4
                                            !WRITE(40, FMT='((A6,I1,A3,A1))', ADVANCE='no') 'Rdest_',xcnt,'   ', ','
                                                        END DO
                                        
                                                END IF
                                    
                                                    
                                                    
                                                !! ---c. FILL IN SEARCHDIR_LO---
                
                                                    !DO xcnt=1, occpts
                                                    !    ! searchdir_lo(source occ, dest occ, agg state)
                                                    !    searchdir_lo(occcnt,xcnt,pcnt)=searchdir_parsl(occcnt, xcnt)*&
                                                    !        (    ((occsearch_elasl*temp_R_destocclo(xcnt, pcnt))/lambdasol   )**(occsearch_elasl/(1.0-occsearch_elasl)))
                                                    !END DO
                                                    
                                                    DO xcnt=1, occpts
                                                        searchdir_lo(occcnt,xcnt,pcnt)=0.0_8
                                                    END DO 
                                                    
                                                    !searchdir_lo(occcnt,:,pcnt)=0.0
                                                    tempreal=0.0        !! total probability assigned
                                                    DO xcnt=1, occpts
                                                        IF((temp_R_destocclo(xcnt,pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) THEN
                                                            searchdir_lo(occcnt,xcnt,pcnt)=searchdir_parsl(occcnt, xcnt)*(((temp_R_destocclo(xcnt, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt,pcnt))**(occsearch_elasl/(1.0-occsearch_elasl)))
                                                            ! the error message below might occur not as an error in the method but rather something that happens 
                                                            ! when ERo_baseline is wrong???
                                                            IF(lambdasol<0.0) WRITE(10,*) 'lambdasol division str neg : error... on', threadnol, 'with nu:', occsearch_elasl
                                                            IF(lambdasol .eq. 0.0) WRITE(10,*) 'lambdasol=0: error... on', threadnol, 'with nu:', occsearch_elasl

                                                            IF(lambdasol<=0.0) THEN
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no') lambdasol, ','
                                                                tempreal2=(((searchdir_parsl(occcnt, xcnt))**(1.0-occsearch_elasl))* &
                                                                        ((temp_R_destocclo(xcnt,pcnt)-temp_ERo_baseline(occcnt,pcnt))/temp_ERo_baseline(occcnt,pcnt)) &
                                                                        )
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no')  tempreal2, ',' 
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no')  tempreal2**(REAL(((1.0_8)/(1.0_8-occsearch_elasl)))), ',' 
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no')  (REAL(1.0)/(1.0-occsearch_elasl)), ',' 
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no')  (1.0-occsearch_elasl), ',' 
                                                                WRITE(10, FMT='(F10.5, A1)', ADVANCE='no')  occsearch_elasl, ',' 
                                                                WRITE(10, FMT='(E10.5, A1)', ADVANCE='no')  temp_R_destocclo(xcnt,pcnt), ',' 
                                                                WRITE(10, FMT='(F10.5, A1)', ADVANCE='no')  temp_ERo_baseline(occcnt, pcnt), ',' 
                                                                WRITE(10, FMT='(F10.5, A1)', ADVANCE='no')  searchdir_parsl(occcnt, xcnt), ','
                                                                WRITE(10, FMT='(F10.5, A1)', ADVANCE='no')  realloclo(occcnt, xcnt), ','
                                                                WRITE(10, FMT='(A1)') ',' 
                                                            END IF 

                                                            IF(lambdasol>0.0) THEN
                                                                searchdir_lo(occcnt,xcnt,pcnt)=MIN(MAX(searchdir_lo(occcnt,xcnt,pcnt)/((lambdasol)**occsearch_elasl), 0.0),1.0)
                                                                tempreal=tempreal+searchdir_lo(occcnt,xcnt,pcnt)
                                                            else
                                                                searchdir_lo(occcnt,xcnt,pcnt)=0.0
                                                                tempreal=tempreal+0.0
                                                            END IF 
                                                        END IF 
                                                    END DO
                                           
                                                    !REAL(8), DIMENSION(occpts, ppts) :: realloclo_temp
                                                    !REAL(8), DIMENSION(occpts, ppts) :: temp_R_destocclo
                                                    !REAL(8), DIMENSION(ppts) :: temp_ERo_baseline
                                                    !REAL(8), DIMENSION(occpts, occpts, ppts) :: temp_occsearchchoice_matrix
                
                                                    temp_occsearchchoice_matrix(occcnt, :, pcnt)=searchdir_lo(occcnt, :, pcnt)
                                                    
                                                    IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1 ) THEN
                                                        DO xcnt=1, occpts
                                                            ! opt. search dir temp
                                                            WRITE(40, FMT='((F10.5, A1))', ADVANCE='no') searchdir_lo(occcnt,xcnt,pcnt), ',' 
                                                        END DO 
                                                        
                                                        !DO xcnt=1, occpts
                                                        !    ! opt. search dir temp
                                                        !    WRITE(40, FMT='((A8,I1,A1, A1))', ADVANCE='no') 'sdirtemp',xcnt, ' ', ','
                                                        !END DO
                                                        !
                                                    END IF
                                    
                                                    !! second loop
                                                    !DO xcnt=1, occpts
                                                    !    searchdir_lo(occcnt,xcnt,pcnt)=searchdir_lo(occcnt,xcnt,pcnt)+(1.0-tempreal)*searchdir_parsl(occcnt, xcnt)
                                                    !END DO
                                                    !
                                                    IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1 ) THEN
                                                        DO xcnt=1, occpts
                                                            ! opt. search dir final
                                                            WRITE(40, FMT='((F10.5, A1))', ADVANCE='no') searchdir_lo(occcnt,xcnt,pcnt), ',' 
                                                            ! continuation per destination occupation
                                                            !DO xcnt=1, occpts
                                                            !    ! opt. search dir final
                                                            !    WRITE(40, FMT='((A9,I1, A1))', ADVANCE='no') 'sdirfinal',xcnt, ','
                                                            !    ! continuation per destination occupation
                                                            !END DO 
                                                            !
                                                        END DO 
                                                    END IF
                                    
                
                    
                                                    
                
                    END DO find_lambdastar_perocc_do    !occcnt source occupation
                
                
                                    
                            
                                    DO occcnt=1, occpts
                                        realloclo_temp(occcnt, pcnt)=searchdir_lo(occcnt,1, pcnt)*temp_R_destocclo(1,pcnt)
                                        tempreal=searchdir_lo(occcnt,1, pcnt)
                                        DO xcnt=2, occpts
                                            realloclo_temp(occcnt, pcnt)=realloclo_temp(occcnt, pcnt)+searchdir_lo(occcnt, xcnt, pcnt)*temp_R_destocclo(xcnt,pcnt)
                                            tempreal=tempreal+searchdir_lo(occcnt, xcnt, pcnt)
                                        END DO
                                    
                                        realloclo_temp(occcnt, pcnt)= - reallc*pveclocal(pcnt) + tempreal* realloclo_temp(occcnt, pcnt) + &
                                                                        (1.0-tempreal)*temp_ERo_baseline(occcnt, pcnt)
                                                            ! reallocation cost that has to be paid in any case
                                                                                     ! expectation value drawing an occupation
                                                                         ! expectation not drawing an occupation, continuation next period
                                    END DO 
                                    
                                    IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1 ) THEN
                                                        DO occcnt=1, occpts
                                                            ! value of reallocating per superocc
                                                            WRITE(40, FMT='((F10.5, A1))', ADVANCE='no') realloclo_temp(occcnt, pcnt), ',' 
                                                        END DO 
                                                        !
                                                        !DO occcnt=1, occpts
                                                        !    ! value of reallocating per superocc
                                                        !    WRITE(40, FMT='((A9,I1, A1))', ADVANCE='no') 'realloclo_temp',occcnt, ','
                                                        !END DO 
                                                        !
                                    END IF
                                    
                
                
                                    !! ---cumulative searchdir_lo---
                                    DO occcnt=1, occpts
                                        !searchdir_lo(occcnt,1, pcnt)
                                        DO xcnt=2, occpts
                                            ! searchdir not cumulative yet (later! don't forget!)
                                            searchdir_lo(occcnt, xcnt, pcnt)=searchdir_lo(occcnt, xcnt, pcnt)+searchdir_lo(occcnt, xcnt-1, pcnt)
                                            IF(searchdir_lo(occcnt,xcnt,pcnt)>1.0+0.0001) THEN 
                                                WRITE(*,*) 'error in searchdir_lo construction'
                                                PAUSE
                                                EXIT
                                            END IF 
                                        END DO
                
                                    END DO
                
                
                                    !! DONE WITH REALLOCATION
                                    !! WRITE ITERATION OUTCOME...
                                    IF(verbose_during_nm_backw_ind_io==1 .AND. pcnt==1 ) THEN
                                                        DO occcnt=1, occpts
                                                        DO xcnt=1, occpts
                                                            ! value of reallocating per superocc
                                                            WRITE(40, FMT='((F10.5, A1))', ADVANCE='no') searchdir_lo(occcnt,xcnt,pcnt), ',' 
                                                            !DO occcnt=1, occpts
                                                            !DO xcnt=1, occpts
                                                            !! value of reallocating per superocc
                                                            !    WRITE(40, FMT='((A6,I1,A1,I1, A1))', ADVANCE='no') 'sdircum',occcnt,'_',xcnt, ','
                                                            !END DO
                                                            !END DO
                                                            !WRITE(40, FMT='(A1)') ',' 
                                                        END DO 
                                                        END DO 
                                                        WRITE(40, FMT='(A1)') ',' 
                                    END IF
                                    
            END DO reall_aggstate_do ! pcnt
                                !!!!!!!!!!!!!!!!!!!!!!!!!!!!



                            ELSE IF (instantreall==1) THEN
                                    DO occcnt=1, occpts
                                    DO pcnt=1, ppts
                                        tempreal6=0.0_8
                                                DO zcnt=1, zpts
                                                        ! this will be the continuation value after reallocating
                                                        ! this needs to be adapted with more directed search across occupations
                                                            tempreal6=znewpdfl(zcnt)*(VUlo(occcnt, pcnt,1, zcnt)+DMaxUlo(occcnt, pcnt,1,zcnt)) +tempreal6            ! figuring out E[REALLOC]
                                                                                                          ! reallocation choice    ! application choice                  ! NOTE,
                                                                                                          !ZERO TENURE IN OCCUPATION, SO NO HC
                                                                                                          ! reallocation choice    ! application choice


                                                END DO
                                                realloclo_temp(occcnt, pcnt)=tempreal6 - reallc*pveclocal(pcnt)
                                    END DO
                                    END DO
    END IF
    ELSE IF(noreall_backwinduct_ind == 1) THEN

                                realloclo_temp=-HUGE(1.0_8)

    END IF noreal_if_v3

                            !~~~~~~~~~~~~~~~~~
                            ! 4) New Unemployment value
                            !~~~~~~~~~~~~~~~~~

IF(noreall_backwinduct_ind .ne. 1) THEN

                           DO occcnt=1, occpts
                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4_v3: &
                                                DO z2cnt=zpts,1,-1

                                                ! inexperienced guy; unemployed so now skill appreciation
                                                  IF(xcnt==1 .AND. (realloclo_temp(occcnt, p2cnt)<(VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                     ! stay in occupation
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                                    !MAX(   REALLOCl(p2cnt),(VUlo(p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt==1 .AND. (realloclo_temp(occcnt,p2cnt)>=(VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8)  ))) THEN
                                                      ! reallocate
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)*(realloclo_temp(occcnt, p2cnt))        +tempreal6
                                                    EXIT zloop_4_v3

                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                      IF((realloclo_temp(occcnt, p2cnt)<(VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8)))  .OR. &
                                                          (realloclo_temp(occcnt, p2cnt)<(VUlo(occcnt, p2cnt, xcnt-1, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt-1, z2cnt), 0.0_8)))  ) THEN

                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      (MAX(   realloclo_temp(occcnt, p2cnt),(VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8) )    )   ) &
                                                                    +skilldepl* &
                                                                    (MAX(   realloclo_temp(occcnt, p2cnt),(VUlo(occcnt, p2cnt, xcnt-1, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt-1, z2cnt), 0.0_8) )    )   ) &
                                                                    ) &
                                                                    +tempreal6
                                                      ELSE IF  ((realloclo_temp(occcnt, p2cnt)>=(VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8)))  .AND. &
                                                          (realloclo_temp(occcnt, p2cnt)>=(VUlo(occcnt, p2cnt, xcnt-1, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt-1, z2cnt), 0.0_8)  ))) THEN


                                                            tempreal6=ptransmatrixl(pcnt, p2cnt)*ztranscdfl(zcnt, z2cnt)* (realloclo_temp(occcnt, p2cnt))        &
                                                                    +tempreal6
                                                            EXIT zloop_4_v3
                                                        END IF
                                                END IF

                                                END DO zloop_4_v3
                                        END DO
                                    VUlotemp(occcnt, pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
                           END DO !occcnt
ELSE IF(noreall_backwinduct_ind == 1) THEN

                           DO occcnt=1, occpts
                           DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                    tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            zloop_4nr_v3: DO z2cnt=zpts,1,-1
                                                  IF(xcnt==1)  THEN
                                                      tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                      (VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8) )        +tempreal6
                                                  !MAX(   REALLOCl(p2cnt),(VUlo(p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(p2cnt, xcnt, z2cnt), 0.0_8)
                                                  ELSE IF (xcnt>1 .AND. xpts>1) THEN

                                                    tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)* &
                                                                    ( &
                                                                    (1.0_8-skilldepl)* &
                                                                      ((VUlo(occcnt, p2cnt, xcnt, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt, z2cnt), 0.0_8) )       ) &
                                                                    +skilldepl* &
                                                                    ((VUlo(occcnt, p2cnt, xcnt-1, z2cnt) +MAX(DMaxUlo(occcnt, p2cnt, xcnt-1, z2cnt), 0.0_8)     )   ) &
                                                                    ) &
                                                                    +tempreal6

                                                  END IF


                                                END DO zloop_4nr_v3
                                        END DO
                                    VUlotemp(occcnt, pcnt, xcnt, zcnt)=b + bt*(1.0_8-dd)*tempreal6
                                    END DO

                                    END DO
                           END DO
                           END DO
END IF


                           !IF(runnumber<=3) WRITE(*,*) 'VU-----', VUltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VU-----', VUltemp(ppts,1, 1:MIN(zpts,10))


                            !~~~~~~~~~~~~~~~~~~~
                            !  5) New Employment value
                            !~~~~~~~~~~~~~~~~~~~~~~

                              DO occcnt=1, occpts
                              DO pcnt=1, ppts
                                DO zcnt=1, zpts
                                    DO xcnt=1, xpts
                                      tempreal6=0.0_8
                                        DO p2cnt=1, ppts
                                            DO z2cnt=1, zpts
                                                DO x2cnt=1, xpts
                                                                       tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                        (delta_vector(xcnt)*VUlo(occcnt, p2cnt, x2cnt, z2cnt)+ (1.0_8-delta_vector(xcnt))*MAX(VUlo(occcnt, p2cnt, x2cnt, z2cnt),(VElo(occcnt, p2cnt, x2cnt, z2cnt))) )  &
                                                                       +tempreal6
                                                END DO
                                            END DO
                                        END DO
                                              VElotemp(occcnt, pcnt, xcnt, zcnt)= prodlo(occcnt, pcnt, xcnt, zcnt) + bt*(1.0_8-dd)*tempreal6
                                    END DO
                                END DO
                              END DO
                              END DO

                            !IF(runnumber<=3) WRITE(*,*) 'VE-----', VEltemp(MAX(ppts/2,1),1,MAX(zpts/2,1):zpts)
                           !IF(runnumber<=20) WRITE(*,*) 'VE-----', VEltemp(ppts,1, 1:MIN(zpts,10))
                           ! IF(runnumber<=100 .AND. runnumber>3) WRITE(*,*) 'R,U,W', Reallocl(ppts), VUltemp(ppts,1, 1), VEltemp(ppts,1, 1)

!----------------------------------------------
!  TEST FOR CONVERGENCE
!---------------------------------------------

tempreal6=MAXVAL(ABS(VUlotemp(:,:,:,:)-VUlo(:,:,:,:))/ABS(REAL(VUlotemp(:,:,:,:))))
tempreal5=MAXVAL(ABS(VElotemp(:,:,:,:)-VElo(:,:,:,:))/ABS(REAL(VElotemp(:,:,:,:))))
!tempreal6=2.0*MAXVAL(ABS(VUlotemp(:,:,:,:)-VUlo(:,:,:,:))/(ABS(REAL(VUlotemp(:,:,:,:)))+ABS(REAL(VUlo(:,:,:,:))))
!tempreal5=2.0*MAXVAL(ABS(VElotemp(:,:,:,:)-VElo(:,:,:,:))/(ABS(REAL(VElotemp(:,:,:,:)))+ABS(REAL(VElo(:,:,:,:))))

convdistance3=MAX(tempreal5,tempreal6)

IF(verbose_nm_backw_ind==1 .AND. runnumber_nm==2) WRITE(10,*) 'runnumber_nm=', runnumber_nm, ' convdistance= ', convdistance3
IF(verbose_nm_backw_ind==1 .AND. (runnumber_nm/1000)*1000-runnumber_nm==0) WRITE(10,*) 'runnumber=', runnumber_nm, ' convdistance= ', convdistance3

!IF(verboseswitch==1) WRITE(*,*) 'runnumber=', runnumber, ' convcrit= ', convdistance
IF(verbose_nm_backw_ind==1 .AND. (runnumber_nm/1000)*1000-runnumber_nm==0) WRITE(*,*) 'runnumber=', runnumber_nm, ' convdistance= ', convdistance3


!----------------------------------------------
! ACCELERATOR:???
!----------------------------------------------

IF(runnumber_nm<=2) THEN
    tempreal=12.0
ELSE IF (runnumber_nm<=10) THEN
    tempreal=12.0
ELSE IF (runnumber_nm<=30) THEN
    tempreal=12.0
ELSE IF (runnumber_nm<=40) THEN
    tempreal=12.0
    END IF
!
!DO occcnt=1, occpts
!    DO pcnt=1, ppts
!        DO xcnt=1, xpts
!            DO zcnt=1, zpts
!                VUlo(occc
IF (runnumber_nm<=40 .AND. valuefct_accel == 1) THEN
    DO occcnt=1, occpts
        DO pcnt=1, ppts
            DO xcnt=1, xpts
                DO zcnt=1, zpts
                    VUlo(occcnt, pcnt, xcnt, zcnt)=VUlotemp(occcnt, pcnt, xcnt, zcnt)+tempreal*(VUlotemp(occcnt, pcnt, xcnt, zcnt)-VUlo(occcnt, pcnt, xcnt, zcnt))
                    VElo(occcnt, pcnt, xcnt, zcnt)=VElotemp(occcnt, pcnt, xcnt, zcnt)+tempreal*(VElotemp(occcnt, pcnt, xcnt, zcnt)-VElo(occcnt, pcnt, xcnt, zcnt))
                END DO
            END DO
            Realloclo(occcnt, pcnt)=Realloclo_temp(occcnt, pcnt)+tempreal*(Realloclo_temp(occcnt, pcnt)-Realloclo(occcnt, pcnt))
        END DO
    END DO
ELSE IF (runnumber_nm>40 .OR. valuefct_accel .ne. 1) THEN
VUlo=VUlotemp
VElo=VElotemp
Realloclo=Realloclo_temp
END IF
!IF(runnumber==2) WRITE(*,*) 'VEl(MAX(ppts/2, 1), 1, MAX(4*zpts)/5,1)', VEl(MAX((ppts/2), 1), 1, MAX((4*zpts)/5,1))


END DO convergence_loop3

IF(verbose_nm_backw_ind==1) THEN
    WRITE(*,*) 'converged and calculated wages, dist_3', convdistance3, 'iter', runnumber_nm
    WRITE(10,*) 'converged and calculated wages, dist_3', convdistance3, 'iter', runnumber_nm
END IF

!!=================================
 !!SET UP DECISION RULES?     WAGES AND CUTOFFS?
!!+===============================

    ! JFO, see above
    ! SEARCHDIR, see above
    ! poliR: follows now
    ! poliDW: follows now


 IF(noreall_backwinduct_ind .ne. 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, zcnt), 0.0_8)+VUl(pcnt,xcnt, zcnt))) THEN
                                                        poliRl(pcnt, xcnt, zcnt)=0              ! 0 is reallocation
                                                ELSE
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        IF((reallocl(pcnt)>(MAX(DmaxUl(pcnt,xcnt, MAX(1,zcnt-1)), 0.0_8)+VUl(pcnt,xcnt, MAX(1,zcnt-1)))) &
                                                                .OR. zcnt==1) THEN
                                                            zrl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliRl(pcnt,xcnt,zpts)==0) zrl(pcnt,xcnt)=zpts+1
                                    END DO
                              END DO

ELSE IF(noreall_backwinduct_ind == 1) THEN
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                        poliRl(pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        zrl(pcnt,xcnt)=1

                                       END DO
                                    END DO
                              END DO
    END IF


                            DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(VEl(pcnt,xcnt, zcnt)<VUl(pcnt,xcnt, zcnt)) THEN
                                                        poliDWl(pcnt, xcnt, zcnt)=0              ! 0 is separation
                                                ELSE
                                                        poliDWl(pcnt, xcnt, zcnt)=1              ! 1 is employment stay
                                                        IF(VEl(pcnt,xcnt, MAX(1,zcnt-1))<VUl(pcnt,xcnt, MAX(1,zcnt-1)) &
                                                                .OR. zcnt==1) THEN
                                                            zsl(pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliDWl(pcnt,xcnt,zpts)==0) zsl(pcnt,xcnt)=zpts+1
                                    END DO
    END DO

!!!=============================
!!! OCCUPATION WIDE SHOCKS
!!!=============================

 IF(noreall_backwinduct_ind .ne. 1) THEN
                              DO occcnt=1, occpts
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(realloclo(occcnt, pcnt)>(MAX(DmaxUlo(occcnt, pcnt,xcnt, zcnt), 0.0_8)+VUlo(occcnt, pcnt,xcnt, zcnt))) THEN
                                                        poliRlo(occcnt, pcnt, xcnt, zcnt)=0              ! 0 is reallocation
                                                ELSE
                                                        poliRlo(occcnt, pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        IF((realloclo(occcnt, pcnt)>(MAX(DmaxUlo(occcnt, pcnt,xcnt, MAX(1,zcnt-1)), 0.0_8)+VUlo(occcnt, pcnt,xcnt, MAX(1,zcnt-1)))) &
                                                                .OR. zcnt==1) THEN
                                                            zrlo(occcnt, pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliRlo(occcnt, pcnt,xcnt,zpts)==0) zrlo(occcnt, pcnt,xcnt)=zpts+1
                                    END DO
                              END DO
                              END DO

    ELSE IF(noreall_backwinduct_ind == 1) THEN
                              DO occcnt=1, occpts
                              DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                        poliRlo(occcnt, pcnt, xcnt, zcnt)=1              ! 1 is occupational stay
                                                        zrlo(occcnt, pcnt,xcnt)=1

                                       END DO
                                    END DO
                              END DO
                              END DO
    END IF

!! separation cutoffs
                            DO occcnt=1, occpts
                            DO pcnt=1, ppts
                                    DO xcnt=1, xpts
                                      DO zcnt=1, zpts
                                                IF(VElo(occcnt, pcnt,xcnt, zcnt)<VUlo(occcnt, pcnt,xcnt, zcnt)) THEN
                                                        poliDWlo(occcnt, pcnt, xcnt, zcnt)=0              ! 0 is separation
                                                ELSE
                                                        poliDWlo(occcnt, pcnt, xcnt, zcnt)=1              ! 1 is employment stay
                                                        IF(VElo(occcnt, pcnt,xcnt, MAX(1,zcnt-1))<VUlo(occcnt, pcnt,xcnt, MAX(1,zcnt-1)) &
                                                                .OR. zcnt==1) THEN
                                                            zslo(occcnt, pcnt,xcnt)=zcnt
                                                        END IF

                                                END IF
                                      END DO
                                          IF(poliDWlo(occcnt, pcnt,xcnt,zpts)==0) zslo(occcnt, pcnt,xcnt)=zpts+1
                                    END DO
                            END DO
                            END DO


!WRITE(*,*) 'reallocation cutoffs', zrl(:,1)
!WRITE(*,*) 'separation cutoffs', zsl(:,1)
!
!!---------------------------
!!  FINDING THE CUTOFFS
!!-------------------------------
!
!reservation_zr(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliR(pcnt,xcnt,1,1)==1)   reservation_zr(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliR(pcnt, xcnt, zcnt,1)==0 .AND. poliR(pcnt, xcnt, zcnt+1,1)==1) reservation_zr(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliR(pcnt, xcnt, zpts,1)==0) reservation_zr(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!
!reservation_zq(:,:)=0
!DO pcnt=1, ppts
!DO xcnt=1, xpts
!   IF(poliDW(pcnt,xcnt,1,1)==1)   reservation_zq(pcnt,xcnt)=1   ! always remaining in the island
!   DO zcnt=1, zpts-1
!        IF(poliDW(pcnt, xcnt, zcnt,1)==0 .AND. poliDW(pcnt, xcnt, zcnt+1,1)==1) reservation_zq(pcnt, xcnt)=zcnt+1
!   END DO
!   IF (poliDW(pcnt, xcnt, zpts,1)==0) reservation_zq(pcnt, xcnt)=zpts+1
!END DO
!END DO
!
!


!!_-_=_   WRITING THE CUTOFFS

!
!!--------------setup the increased precision
!
!
!tempcounter1=0          ! tempcounter1 is going to keep track of the highest possible reallocation
!tempcounter2=zpts       ! tempcounter2 is going to keep track of the lowest possible staying with a job
!
!DO pcnt=1, ppts
!    DO xcnt=1, xpts
!        DO zcnt=1, zpts
!            IF((poliDW(pcnt, xcnt, zcnt,2)==0 .OR. poliR(pcnt, xcnt, zcnt,2)==0) .AND. zcnt>tempcounter1) tempcounter1=zcnt
!            IF((poliDW(pcnt, xcnt, zcnt,2)==1 .OR. poliR(pcnt, xcnt, zcnt,2)==1) .AND. zcnt<tempcounter2) tempcounter2=zcnt
!        END DO
!    END DO
!END DO
!
!WRITE(*,*) 'HIGHEST POSSIBLE REALLOCATION OCCURS AT: ', tempcounter1
!WRITE(*,*) 'LOWEST POSSIBLE STAYING, OCCURS AT: ', tempcounter2
!
!IF (tempcounter2>2) zprecision_lb=zvector(MAX(tempcounter2-2, 1))
!IF (tempcounter1<zpts-1) zprecision_ub=zvector(MIN(tempcounter1+10, zpts))
!
!

!----------------reset current period value and policy functions
!IF(tworunnumber==1 .AND. tworun_switch==1) THEN
!    poliR(:,:,1)=0
!    poliDW(:,:,:, 1)=0
!    VEl(:,:,:, 1)=0.0_8
!    VUl(:,:,1)=0.0_8
!    jfl(:,:,1)=0.0_8
!    EV(:,:,1)=0.0_8
!    DmaxUl(:,:,1)=0.0_8
!    REALLOC(:,1)=0.0_8
!END IF

                           !------------------------------------------------------
                           ! 7) calculate WAGE PAYMENTS after convergence
                           !-----------------------------------------------------



        DO pcnt=1, ppts
            DO zcnt=1, zpts
                DO xcnt=1, xpts
                    tempreal6=0.0_8
                    ! first, calculate the continuation value
                    DO z2cnt=1, zpts
                        DO p2cnt=1, ppts
                           DO x2cnt=1, xpts
                                        tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                      ((  &
                                                                        (       (  delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDWl(p2cnt, x2cnt, z2cnt)) )  * VUl(p2cnt, x2cnt, z2cnt)       &
                                                                        ! +1(
                                                                       +      (1-delta_vector(xcnt))*poliDWl(p2cnt, x2cnt, z2cnt)*&                 ! 0 (open
                                                                        (    (1-eta)*VEl(p2cnt,x2cnt,z2cnt) + eta*VUl(p2cnt,x2cnt, z2cnt)       )   )  &               ! -1(
                                                                        )) &
                                                                       +tempreal6

!                                        tempreal6=ptransmatrix(pcnt, p2cnt)*ztransmatrix(zcnt, z2cnt)*&
!                                                                       ( (delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDW(p2cnt, xcnt, z2cnt, 2)))*VUl(p2cnt, z2cnt, 2) + (1-delta_vector(xcnt))*poliDW(p2cnt, xcnt,
!                                                                            z2cnt, 2)*&
!                                                                        (VEl(p2cnt, xcnt, z2cnt, 2)-k*( jfl(p2cnt, z2cnt, 2)**((1.0_8-eta)/eta) )) )  &
!                                                                       +tempreal6
                            END DO
                        END DO
                     END DO
                    wagel(pcnt, xcnt, zcnt)=(1.0_8-eta)*VEl(pcnt, xcnt, zcnt)+eta*VUl(pcnt, xcnt, zcnt) -bt*(1-dd)*tempreal6
                    !WAGE(pcnt, xcnt, zcnt,1)=VEl(pcnt, xcnt, zcnt)-k*( jfl(pcnt, zcnt)**((1.0_8-eta)/eta) ) -bt*tempreal6
               END DO
            END DO
    END DO

!! WAGES PER OCCUPATION
        DO occcnt=1, occpts
        DO pcnt=1, ppts
            DO zcnt=1, zpts
                DO xcnt=1, xpts
                    tempreal6=0.0_8
                    ! first, calculate the continuation value
                    DO z2cnt=1, zpts
                        DO p2cnt=1, ppts
                           DO x2cnt=1, xpts
                                        tempreal6=ptransmatrixl(pcnt, p2cnt)*ztransmatrixl(zcnt, z2cnt)*xtransmatrixl(xcnt, x2cnt)*&
                                                                      ((  &
                                                                        (       (  delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDWlo(occcnt, p2cnt, x2cnt, z2cnt)) )  * VUlo(occcnt, p2cnt, x2cnt, z2cnt)       &
                                                                        ! +1(
                                                                       +      (1-delta_vector(xcnt))*poliDWlo(occcnt, p2cnt, x2cnt, z2cnt)*&                 ! 0 (open
                                                                        (    (1-eta)*VElo(occcnt, p2cnt,x2cnt,z2cnt) + eta*VUlo(occcnt, p2cnt,x2cnt, z2cnt)       )   )  &               ! -1(
                                                                        )) &
                                                                       +tempreal6

!                                        tempreal6=ptransmatrix(pcnt, p2cnt)*ztransmatrix(zcnt, z2cnt)*&
!                                                                       ( (delta_vector(xcnt)+ (1-delta_vector(xcnt))*(1-poliDW(p2cnt, xcnt, z2cnt, 2)))*VUl(p2cnt, z2cnt, 2) + (1-delta_vector(xcnt))*poliDW(p2cnt, xcnt,
!                                                                            z2cnt, 2)*&
!                                                                        (VEl(p2cnt, xcnt, z2cnt, 2)-k*( jfl(p2cnt, z2cnt, 2)**((1.0_8-eta)/eta) )) )  &
!                                                                       +tempreal6
                            END DO
                        END DO
                     END DO
                    wagelo(occcnt, pcnt, xcnt, zcnt)=(1.0_8-eta)*VElo(occcnt, pcnt, xcnt, zcnt)+eta*VUlo(occcnt, pcnt, xcnt, zcnt) -bt*(1-dd)*tempreal6
                    !WAGE(pcnt, xcnt, zcnt,1)=VEl(pcnt, xcnt, zcnt)-k*( jfl(pcnt, zcnt)**((1.0_8-eta)/eta) ) -bt*tempreal6
               END DO
            END DO
        END DO
        END DO


!IF(verbose_nm_backw_ind==1) WRITE(*,*) 'converged and calculated wages, distance2', convdistance2, 'iteration', runnumber
!P


!! A2closed. CLOSE FILE TO WRITE TO 
    IF(verbose_during_nm_backw_ind_io==1) THEN
        !WRITE(*,*) 'thread reporting', i1
        CLOSE(40)
    END IF 
    
    
    



    !=======================================
    !  WRITE DOWN VALUE FUNCTIONS AND DECISION RULES
    !==============================================


IF(verbose_backw_ind_io==1) THEN



    !WRITE(*,*) 'thread reporting', i1
WRITE(x1,FMT='(I3.3)') threadnol !

filename='decrule_vf_ind'//trim(x1)//'.csv'
OPEN(UNIT=39, file=filename, form='formatted', status='replace')

write(39, FMT='(A4, A1)' , ADVANCE='NO') 'pidx', ','
write(39, FMT='(A4, A1)' , ADVANCE='NO') 'xidx', ','
write(39, FMT='(A4, A1)' , ADVANCE='NO') 'zidx', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'p_level', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'x_level', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_level', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'prod', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'wage', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'VE', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'VU', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'poliDW', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'poliR', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_r_binary', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_s_binary', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_r_func', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_s_func', ','

write(39, FMT='(A1)' ) ','



    !                                        epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
    !                                        epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim
    !

DO pcnt=1,ppts
DO xcnt=1, xpts
DO zcnt=1, zpts

write(39, FMT='(I4, A1)' , ADVANCE='NO') pcnt, ','
write(39, FMT='(I4, A1)' , ADVANCE='NO') xcnt, ','
write(39, FMT='(I4, A1)' , ADVANCE='NO') zcnt, ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') pvector_in(pcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') xvector_in(xcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') zvector_in(xcnt), ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') prodl(pcnt, xcnt, zcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') wagel(pcnt,xcnt,zcnt), ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') VEl(pcnt, xcnt, zcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') VUl(pcnt, xcnt, zcnt), ','

write(39, FMT='(I12, A1)' , ADVANCE='NO') poliDWl(pcnt, xcnt, zcnt), ','
write(39, FMT='(I12, A1)' , ADVANCE='NO') poliRl(pcnt, xcnt,zcnt), ','

! EMPLOYMENT PER AGE
!! cutoffs
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliRl(pcnt,xcnt, 1)==1) THEN
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliRl(pcnt,xcnt, zcnt-1)==0 .AND. poliRl(pcnt,xcnt, zcnt)==1) THEN
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! separation cutoff
IF(zcnt==1) THEN
    ! poliR ==1 is occ stay
    IF(poliDWl(pcnt,xcnt, 1)==1) THEN
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
    IF(poliDWl(pcnt,xcnt, zcnt-1)==0 .AND. poliDWl(pcnt,xcnt, zcnt)==1) THEN
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 1.0, ','
    ELSE
        write(39, FMT='(F12.6, A1)', ADVANCE='NO') 0.0, ','
    END IF
END IF

!! cutoff z-values (as a function of p,x)
write(39, FMT='(I12, A1)' , ADVANCE='NO') zrl(pcnt, xcnt), ','
write(39, FMT='(I12, A1)' , ADVANCE='NO') zsl(pcnt, xcnt), ','
write(39, FMT='(A1)' ) ','

END DO  !zcnt
END DO  !xcnt
END DO  ! pcnt

CLOSE(39)



END IF


IF(verbose_nm_backw_ind_io==1) THEN



    !WRITE(*,*) 'thread reporting', i1
WRITE(x1,FMT='(I3.3)') threadnol !

filename='decrule_vf_occ'//trim(x1)//'.csv'
OPEN(UNIT=39, file=filename, form='formatted', status='replace')

write(39, FMT='(A4, A1)' , ADVANCE='NO') 'pidx', ','
write(39, FMT='(A4, A1)' , ADVANCE='NO') 'xidx', ','
write(39, FMT='(A4, A1)' , ADVANCE='NO') 'oidx', ','
write(39, FMT='(A4, A1)' , ADVANCE='NO') 'zidx', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'p_level', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'x_level', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'o_level', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_level', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'prod_occ', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'wage_occ', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'VEO', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'VUO', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'ERO', ','

write(39, FMT='(A12, A1)' , ADVANCE='NO') 'poliDWo', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'poliRo', ','
!write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchdir_oc', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_ro_binary', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_so_binary', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_ro_func', ','
WRITE(39, FMT='(A12, A1)' , ADVANCE='NO') 'z_so_func', ','


!! search elasticity
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchelas', ','
    !! search parameters
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchpar_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchpar_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchpar_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchpar_4', ','
    !! search probability per occupation
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchprob_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchprob_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchprob_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchprob_4', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'tot_srchprob', ','    
!! alternative search probability
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Asearchprob1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Asearchprob2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Asearchprob3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Asearchprob4', ','
!! search INTENSITIES (should sum up to 1)
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchint_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchint_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchint_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchint_4', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'tot_srchint', ','    

!! marginal search condition (probability only)
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'mrgsrchprob1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'mrgsrchprob2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'mrgsrchprob3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'mrgsrchprob4', ','

    !! values of reallocating to other occupations
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Rdest_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Rdest_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Rdest_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'Rdest_4', ','
!! continuation value when not matched with an occupation
write(39, FMT='(A1, A1)', ADVANCE='NO') ',' , ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'cntvalnoocc1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'cntvalnoocc2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'cntvalnoocc3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'cntvalnoocc4', ','
!! first order condition (including value fucntions, need to be equated) 
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchfoc_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchfoc_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchfoc_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'searchfoc_4', ','
!!



!! expected value of reallocation (by source occ)
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'ER_1', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'ER_2', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'ER_3', ','
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'ER_4', ','
!! lambdasol
write(39, FMT='(A12, A1)' , ADVANCE='NO') 'lambdasol_p', ','
write(39, FMT='(A1)' ) ','



    !                                        epz_xallp_distribution_sim, epz_xallp_young_distribution_sim, epz_xallp_prime_distribution_sim, &
    !                                        epz_1yb4_x_distribution_sim, sep_epz_1yb4_x_distribution_sim
    !

DO pcnt=1,ppts
DO xcnt=1, xpts
DO zcnt=1, zpts

DO occcnt=0, occpts


write(39, FMT='(I4, A1)' , ADVANCE='NO') pcnt, ','
write(39, FMT='(I4, A1)' , ADVANCE='NO') xcnt, ','
write(39, FMT='(I4, A1)' , ADVANCE='NO') occcnt, ','
write(39, FMT='(I4, A1)' , ADVANCE='NO') zcnt, ','

IF(occcnt>0) THEN
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') pvector_in(pcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') xvector_in(xcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') occprodl(occcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') zvector_in(zcnt),  ','

    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') prodlo(occcnt, pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') wagelo(occcnt, pcnt,xcnt,zcnt), ','

    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') VElo(occcnt, pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') VUlo(occcnt, pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') realloclo(occcnt, pcnt), ','

    write(39, FMT='(I12, A1)' , ADVANCE='NO') poliDWlo(occcnt, pcnt, xcnt, zcnt), ','
    write(39, FMT='(I12, A1)' , ADVANCE='NO') poliRlo(occcnt, pcnt, xcnt,zcnt), ','
    !tempint=MIN(occpts, zcnt-((zcnt-1)/occpts)*occpts)
    !write(39, FMT='(F16.9, A1)' , ADVANCE='NO') searchdir_lo(occcnt, tempint, pcnt), ','
    ! EMPLOYMENT PER AGE
    !! cutoffs
    IF(zcnt==1) THEN
        ! poliR ==1 is occ stay
        IF(poliRlo(occcnt, pcnt,xcnt, 1)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
        IF(poliRlo(occcnt, pcnt,xcnt, zcnt-1)==0 .AND. poliRlo(occcnt, pcnt,xcnt, zcnt)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    END IF

    !! separation cutoff
    IF(zcnt==1) THEN
        ! poliR ==1 is occ stay
        IF(poliDWlo(occcnt, pcnt,xcnt, 1)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
        IF(poliDWlo(occcnt, pcnt,xcnt, zcnt-1)==0 .AND. poliDWlo(occcnt, pcnt,xcnt, zcnt)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    END IF

    !! cutoff z-values (as a function of p,x)
    write(39, FMT='(I12, A1)' , ADVANCE='NO') zrlo(occcnt, pcnt, xcnt), ','
    write(39, FMT='(I12, A1)' , ADVANCE='NO') zslo(occcnt, pcnt, xcnt), ','
    !!------------------------------------
    !! search parameters
    !!------------------------------------
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl, ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') searchdir_parsl(occcnt, 1), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') searchdir_parsl(occcnt, 2), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') searchdir_parsl(occcnt, 3), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') searchdir_parsl(occcnt, 4), ','
    !!------------------------------------
    !! search probability per occupation
    !!------------------------------------
    tempreal=searchdir_lo(occcnt, 1, pcnt)
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal, ','
    tempreal2=searchdir_lo(occcnt, 2, pcnt)-searchdir_lo(occcnt, 1, pcnt)
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal2, ','
    tempreal3=searchdir_lo(occcnt, 3, pcnt)-searchdir_lo(occcnt, 2, pcnt)
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal3, ','
    tempreal4=searchdir_lo(occcnt, 4, pcnt)-searchdir_lo(occcnt, 3, pcnt)
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal4, ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') searchdir_lo(occcnt, 4, pcnt), ','
    !! marginal search condition (probability only)
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*((tempreal/searchdir_parsl(occcnt, 1))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*((tempreal2/searchdir_parsl(occcnt, 2))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*((tempreal3/searchdir_parsl(occcnt, 3))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*((tempreal4/searchdir_parsl(occcnt, 4))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    ! marg search to dest occ1: searchdir_prsl(occcnt,1) * ((R-ERbase)/ERbase)**(occsearch_elasl/(1.0-occsearch_elasl))*lambdasol_pvector(pcnt)**-occsearch_elasl

    !!------------------------------------        
    !! recalculated search probabilities 
    !!------------------------------------
    tempreal5=-1.0
    tempreal6=-1.0
    tempreal7=-1.0
    tempreal8=-1.0

    IF((temp_R_destocclo(1, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal5= searchdir_parsl(occcnt,1) * (((temp_R_destocclo(1, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(occsearch_elasl/(1.0-occsearch_elasl)))* &
                                                    (lambdasol_opvector(occcnt,pcnt))**(-occsearch_elasl)
    IF((temp_R_destocclo(2, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal6= searchdir_parsl(occcnt,2) * (((temp_R_destocclo(2, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(occsearch_elasl/(1.0-occsearch_elasl)))* &
                                                    (lambdasol_opvector(occcnt,pcnt))**(-occsearch_elasl)
    IF((temp_R_destocclo(3, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal7= searchdir_parsl(occcnt,3) * (((temp_R_destocclo(3, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(occsearch_elasl/(1.0-occsearch_elasl)))* &
                                                    (lambdasol_opvector(occcnt,pcnt))**(-occsearch_elasl)
    IF((temp_R_destocclo(4, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal8= searchdir_parsl(occcnt,4) * (((temp_R_destocclo(4, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(occsearch_elasl/(1.0-occsearch_elasl)))* &
                                                    (lambdasol_opvector(occcnt,pcnt))**(-occsearch_elasl)
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal5,','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal6,','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal7, ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal8, ','
    !!------------------------------------
    !! search intensities (redefine tempreal5-8)
    !!------------------------------------
    tempreal5=-1.0
    tempreal6=-1.0
    tempreal7=-1.0
    tempreal8=-1.0

    IF((temp_R_destocclo(1, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal5= searchdir_parsl(occcnt,1) * (((temp_R_destocclo(1, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(1.0/(1.0-occsearch_elasl)))* &
                                                    ((1.0/lambdasol_opvector(occcnt,pcnt)))
    
    IF((temp_R_destocclo(2, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal6= searchdir_parsl(occcnt,2) * (((temp_R_destocclo(2, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(1.0/(1.0-occsearch_elasl)))* &
                                                    ((1.0/lambdasol_opvector(occcnt,pcnt)))
    
    IF((temp_R_destocclo(3, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal7= searchdir_parsl(occcnt,3) * (((temp_R_destocclo(3, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(1.0/(1.0-occsearch_elasl)))* &
                                                    ((1.0/lambdasol_opvector(occcnt,pcnt)))
    
    IF((temp_R_destocclo(4, pcnt)-temp_ERo_baseline(occcnt, pcnt))>0.0) &
    tempreal8= searchdir_parsl(occcnt,4) * (((temp_R_destocclo(4, pcnt)-temp_ERo_baseline(occcnt, pcnt))/temp_ERo_baseline(occcnt, pcnt))**(1.0/(1.0-occsearch_elasl)))* &
                                                    ((1.0/lambdasol_opvector(occcnt,pcnt)))
    
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal5,','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal6,','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal7, ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal8, ','
    
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') tempreal5+tempreal6+tempreal7+tempreal8, ','
    !!------------------------------------
    !! marginal intensities
    !!------------------------------------
    IF ( (tempreal/tempreal5) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal/tempreal5), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal2/tempreal6) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal2/tempreal6), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal3/tempreal7) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal3/tempreal7), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal4/tempreal8) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal4/tempreal8), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

   
    !!------------------------------------
    !! reallocation values per destination occ (given source occ)
    !!------------------------------------
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(1, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(2, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(3, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(4, pcnt), ','
    !! first order condition (including value fucntions, need to be equated) 
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(1, pcnt)*occsearch_elasl*((tempreal/searchdir_parsl(occcnt, 1))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(2, pcnt)*occsearch_elasl*((tempreal2/searchdir_parsl(occcnt, 2))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(3, pcnt)*occsearch_elasl*((tempreal3/searchdir_parsl(occcnt, 3))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_R_destocclo(4, pcnt)*occsearch_elasl*((tempreal4/searchdir_parsl(occcnt, 4))**((occsearch_elasl-1.0)/occsearch_elasl)), ','
    !!------------------------------------
    !! fall back value when no matching with occupation
    !!------------------------------------
    write(39, FMT='(A1, A1)', ADVANCE='NO') ',' , ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_ERo_baseline(1, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_ERo_baseline(2, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_ERo_baseline(3, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') temp_ERo_baseline(4, pcnt), ','
    !!------------------------------------
    !! first order conditions 
    !!------------------------------------
    IF ( (tempreal/tempreal5) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal/tempreal5)*(temp_R_destocclo(1, pcnt)-temp_ERo_baseline(occcnt, pcnt)), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal2/tempreal6) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal2/tempreal6)*(temp_R_destocclo(2, pcnt)-temp_ERo_baseline(occcnt, pcnt)), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal3/tempreal7) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal3/tempreal7)*(temp_R_destocclo(3, pcnt)-temp_ERo_baseline(occcnt, pcnt)), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

    IF ((tempreal4/tempreal8) >0.0) THEN
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') occsearch_elasl*(tempreal4/tempreal8)*(temp_R_destocclo(4, pcnt)-temp_ERo_baseline(occcnt, pcnt)), ','
    ELSE
        write(39, FMT='(F16.9, A1)', ADVANCE='NO') -1.0, ','
    END IF 

   

    !!------------------------------------
    !! expected reallocation values per source occ 
    !!------------------------------------
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') realloclo(1, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') realloclo(2, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') realloclo(3, pcnt), ','
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') realloclo(4, pcnt), ','
    !!------------------------------------
    !! lambdasol value
    !!------------------------------------
    write(39, FMT='(F16.9, A1)', ADVANCE='NO') lambdasol_opvector(occcnt,pcnt), ','
    write(39, FMT='(A1)' ) ','



ELSEIF(occcnt==0) THEN
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') pvector_in(pcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') xvector_in(xcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') -1.0, ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') zvector_in(zcnt),  ','

    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') prodl(pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') wagel(pcnt,xcnt,zcnt), ','

    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') VEl(pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') VUl(pcnt, xcnt, zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') reallocl(pcnt), ','

    write(39, FMT='(I12, A1)' , ADVANCE='NO') poliDWl(pcnt, xcnt, zcnt), ','
    write(39, FMT='(I12, A1)' , ADVANCE='NO') poliRl(pcnt, xcnt,zcnt), ','
    write(39, FMT='(F16.9, A1)' , ADVANCE='NO') -1.0, ','
    ! EMPLOYMENT PER AGE
    !! cutoffs
    IF(zcnt==1) THEN
        ! poliR ==1 is occ stay
        IF(poliRl(pcnt,xcnt, 1)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
        IF(poliRl(pcnt,xcnt, zcnt-1)==0 .AND. poliRl(pcnt,xcnt, zcnt)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    END IF

    !! separation cutoff
    IF(zcnt==1) THEN
        ! poliR ==1 is occ stay
        IF(poliDWl(pcnt,xcnt, 1)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    ELSEIF(zcnt>1 .AND. zcnt<=zpts) THEN
        IF(poliDWl(pcnt,xcnt, zcnt-1)==0 .AND. poliDWl(pcnt,xcnt, zcnt)==1) THEN
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 1.0, ','
        ELSE
            write(39, FMT='(F16.9, A1)', ADVANCE='NO') 0.0, ','
        END IF
    END IF

    !! cutoff z-values (as a function of p,x)
    write(39, FMT='(I12, A1)' , ADVANCE='NO') zrl(pcnt, xcnt), ','
    write(39, FMT='(I12, A1)' , ADVANCE='NO') zsl(pcnt, xcnt), ','
    write(39, FMT='(A1)' ) ','





END IF

END DO  ! occcnt
END DO  !zcnt
END DO  !xcnt
END DO  ! pcnt

CLOSE(39)



END IF


    CONTAINS


            !---------------------------
            !  MATCHING FUNCTIONS
            !---------------------------

            ! Want to set up matching as a function of J/k

            ! jobfinding rate with Cobb-Douglas matching (incorporating zero profit)
            REAL(8) FUNCTION matchprobfunc(jkratio)
            REAL(8), INTENT(IN) :: jkratio
            matchprobfunc= mc**((2-eta)/(1-eta))*(jkratio)**(eta/(1-eta))
            END FUNCTION matchprobfunc

            ! job finding rate
            REAL(8) FUNCTION pf(theta)
            REAL(8), INTENT(IN) :: theta
            pf=mc*(theta)**eta
            END FUNCTION pf

            ! vacancy filling rate
            REAL(8) FUNCTION qf(theta)
            REAL(8), INTENT(IN) :: theta
                qf=(mc*1._8)/(1.+theta)
            END FUNCTION qf


            !--------------------------------------
            !       PRODUCTION FUNCTION
            !---------------------------------------

            REAL(8) FUNCTION OPROD_FUNC(occid, aggid)
            INTEGER, INTENT(IN)         :: occid
            INTEGER, INTENT(IN)         :: aggid

            oprod_func= (occprodl(occid)*pvector_in(aggid))**occaggprod_elasl(occid)

            END FUNCTION OPROD_FUNC


            REAL(8) FUNCTION FSUMLAMBDA(lambdal, searchdir_parsl_in, realloclo_in)
            REAL(8), INTENT(in) :: lambdal
            REAL(8), DIMENSION(:) :: realloclo_in, searchdir_parsl_in
            !INTEGER, INTENT(in) :: source_occ_ind !, agg_ind
            INTEGER             :: dest_occ_ind
            REAL(8)             :: tempreal
            !occsearch_elas
            !searchdir_pars


            tempreal=0.0_8
            DO dest_occ_ind=1, occpts
                tempreal=tempreal+searchdir_parsl_in(dest_occ_ind)*&
                        ((occsearch_elasl*realloclo_in(dest_occ_ind)/lambdal   )**(1.0/(1.0-REAL(occsearch_elasl))))
            END DO
            FSUMLAMBDA=tempreal

            END FUNCTION FSUMLAMBDA



    END SUBROUTINE ctv_backwards_induct_nm

END MODULE mod_backward_ctv
