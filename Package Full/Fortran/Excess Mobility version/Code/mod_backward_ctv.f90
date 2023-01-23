
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


    SUBROUTINE ctv_backwards_induct(VEl, VUl, zrl, zsl, poliDWl, poliRl, jfl, wagel, &
            prodl, ztransmatrixl, ptransmatrixl, xtransmatrixl, znewpdfl, &
            & threadnol, skilldepl, pvectorin, deltalowhcl, deltahighhcl)

            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VEl
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: VUl
            INTEGER, DIMENSION(:,:), INTENT(inout) :: zrl
            INTEGER, DIMENSION(:,:), INTENT(inout) :: zsl
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: poliDWl
            INTEGER, DIMENSION(:,:,:), INTENT(inout) :: poliRl
            REAL(8), DIMENSION(:,:,:), INTENT(inout) :: jfl
            REAL(8), DIMENSION(ppts), INTENT(in) :: pvectorin
            REAL(8), INTENT(IN) :: deltahighhcl, deltalowhcl
            INTEGER, INTENT(IN) :: threadnol
            
            !REAL(8), DIMENSION(:), INTENT(in): zvector
            !REAL(8), DIMENSION(:), INTENT(in): xvector

            REAL(8), DIMENSION(ppts,xpts,zpts), INTENT(inout) :: wagel
            REAL(8), DIMENSION(ppts,xpts,zpts), INTENT(in):: prodl
            REAL(8), DIMENSION(ppts,ppts), INTENT(in):: ptransmatrixl
            REAL(8), DIMENSION(zpts,zpts), INTENT(in):: ztransmatrixl
            
            REAL(8), DIMENSION(xpts,xpts), INTENT(in):: xtransmatrixl

            REAL(8), DIMENSION(zpts), INTENT(in) :: znewpdfl
            REAL(8), INTENT(IN) :: skilldepl
            
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
    INTEGER :: runnumber
    INTEGER :: zcnt, pcnt, p2cnt, xcnt, z2cnt, x2cnt,cntr, howard_no, tempint
    REAL(8) :: convdistance, convdistance2, convdistance_pol
    REAL(8) :: thetatemp, tempreal, tempreal2, tempreal3, tempreal4, tempreal5, tempreal6

    REAL(8), DIMENSION(ppts,xpts,zpts) :: DmaxUl, VUltemp, VEltemp
    REAL(8), DIMENSION(ppts) :: reallocl

    REAL(8) :: t11, t22
    INTEGER, PARAMETER :: test_verbose_ind=0




    !***************************************************************
    ! 0. SET UP AUXILIARY VARS
    !***************************************************************

    IF(cyclical_reallc_ind .ne. 1) THEN
                pveclocal=1.0_8
    ELSE IF (cyclical_reallc_ind .eq. 1) THEN                                                             
                pveclocal=pvectorin
                
                
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

    !--------------------------------------------------------------
    !   LAST PERIOD + HOWARD's IMPROVEMENT ALGORITHM ITERATIONS
    !--------------------------------------------------------------

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

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') pvectorin(pcnt), ','
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

write(38, FMT='(F12.6, A1)' , ADVANCE='NO') pvectorin(pcnt), ','
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
    
    
    
    !=======================================
    !  WRITE DOWN VALUE FUNCTIONS AND DECISION RULES 
    !==============================================
    
    
IF(verbose_backw_ind==1) THEN



    !WRITE(*,*) 'thread reporting', i1
WRITE(x1,FMT='(I3.3)') threadnol ! 

filename='backw_ind'//trim(x1)//'.csv'
OPEN(UNIT=39, file=filename, form='formatted', status='replace')

write(39, FMT='(A20, A1)' , ADVANCE='NO') 'p_index', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'x_index', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_index', ','

write(39, FMT='(A20, A1)' , ADVANCE='NO') 'p_level', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'x_level', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_level', ','

write(39, FMT='(A20, A1)' , ADVANCE='NO') 'prod', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'wage', ','

write(39, FMT='(A20, A1)' , ADVANCE='NO') 'VE', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'VU', ','

write(39, FMT='(A20, A1)' , ADVANCE='NO') 'poliDW', ','
write(39, FMT='(A20, A1)' , ADVANCE='NO') 'poliR', ','
WRITE(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_binary', ','
WRITE(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_binary', ','
WRITE(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_r_func', ','
WRITE(39, FMT='(A20, A1)' , ADVANCE='NO') 'z_s_func', ','

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

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') pvectorin(pcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') xcnt, ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') zcnt, ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') prodl(pcnt, xcnt, zcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') wagel(pcnt,xcnt,zcnt), ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') VEl(pcnt, xcnt, zcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') VUl(pcnt, xcnt, zcnt), ','

write(39, FMT='(F12.6, A1)' , ADVANCE='NO') poliDWl(pcnt, xcnt, zcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') poliRl(pcnt, xcnt,zcnt), ','

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
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') zrl(pcnt, xcnt), ','
write(39, FMT='(F12.6, A1)' , ADVANCE='NO') zsl(pcnt, xcnt), ','
write(39, FMT='(A1)' ) ','   
 
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
            !               CROSS-TERMS ARE IMPORTANT: additive or multiplicative
            !---------------------------------------


            !REAL(8) FUNCTION matchprobfunc(jkratio, eta)
            !REAL(8), INTENT(IN) :: eta
            !REAL(8), INTENT(IN) :: jkratio
            !matchprobfunc= mc**((2-eta)/(1-eta))*(jkratio)**(eta/(1-eta))
            !END FUNCTION matchprobfunc
            !
            !! job finding rate
            !REAL(8) FUNCTION pf(theta)
            !REAL(8), INTENT(IN) :: theta
            !pf=mc*(theta)**eta
            !END FUNCTION pf
            !
            !! vacancy filling rate
            !REAL(8) FUNCTION qf(theta)
            !REAL(8), INTENT(IN) :: theta
            !    qf=(mc*1._8)/(1.+theta)
            !END FUNCTION qf



    END SUBROUTINE ctv_backwards_induct

END MODULE mod_backward_ctv
