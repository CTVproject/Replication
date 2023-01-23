!**************************************************************************************************
!  CONTAINS BOTH TAUCHEN (immediately below) AND ROUWENHORST (scroll down)
!***************************************************************************************************

!    SUBROUTINE sub_tauchen(n,lambda,rho,sigmaeps,xgrid,gama, precisionind)
    ! n grid
    ! lambda -- highest, lowest grid point
    ! rho, sigmaeps -- AR parameters
    ! xgrid -- grid of shock states
    ! gama -- transition matrix

!   subroutine Rouwenhorst(rho, mu_eps, sigma_eps, N, zvect, Pmat)
    ! rhom, sigma_eps, mu_eps -- AR parameters
    ! N gridpoints
    ! zvect -- grid of shock states
    ! Pmat -- transition matrix

    ! ADDED: inside the subroutine: lognormal_ind: which takes the exponent of the grid points if == 1

!************************************************************************************************
! This subroutine uses the Tauchen'86 method to approximate a continuous AR(1) process in terms of
! a finite-state (n-state) Markov chain. Notice that the sigma input for epsilon is
! standard deviation, not variance!
!
! The process we are discretizing is assumed to be: log x' = rho*log x + epsilon, eps ~ N(0,sigmaeps)
! and in particular, there is NO constant in the AR(1) that measures (1-rho)*mu, like in the Tauchen
! paper. So unconditional mean of log x is assumed to be 0. At the end, the Markov chain is
! converted to levels from logs, so that what we get out is the states and distribution on x, not
! log x. If the initial AR(1) is given in levels, remove that last step.
!
! Below is also the subroutine gamastar for finding the invariant distribution associated with gama
! produced by the Tauchen method.
!************************************************************************************************

MODULE mod_tauchen_ctv

!USE ANORDF_INT

IMPLICIT NONE

CONTAINS

    SUBROUTINE sub_tauchen(n,lambda,rho,sigmaeps,xgrid,gama, precisionind)



        !Dummy variables
        REAL(8), INTENT(in) :: rho, sigmaeps    !AR persistence param. and stdev of AR error term
        INTEGER, INTENT(in) :: n, precisionind
        REAL(8), INTENT(in) :: lambda        !Number of states in Markov chain; no of sd's to top state
        INTEGER :: i, j, k

        !Local variables
        REAL(8) :: stdevx, ub, lb, sumgammarow    !Unconditional stdev of the variable; intermediates
        INTEGER :: middle_n

        !Output
        REAL(8), DIMENSION(n), INTENT(out) :: xgrid        !Grid of shock states
        REAL(8), DIMENSION(n,n), INTENT(out) :: gama    !Resulting transition matrix

        REAL(8) :: temp_prob, temp_expectation, temp_ac

        !Make vector of discrete states: y_N=lambda*(uncond'l st. dev. of the variable)


        stdevx=sigmaeps/((1.0_8-rho**2.0_8)**(0.5_8))

        xgrid(1) = -stdevx*lambda
        xgrid(n) = stdevx*lambda

        ! POPULATE THE X-GRID

        !WRITE(*,*) 'inside tauchen'

        IF(precisionind==1) THEN           ! extra precision at the bottom
            IF (n<8) THEN
                WRITE(*,*) 'too little gridpoints for precision, paused in sub_tauchen'
                !STOP 
            END IF

            middle_n=INT((3*n)/4)

            xgrid(middle_n)= (xgrid(n)+xgrid(1))/2.0_8

            DO i=2,middle_n-1
                xgrid(i)=xgrid(1)+(real(i)-1.0_8)*(xgrid(middle_n)-xgrid(1))/(real(middle_n)-1.0_8)
            ENDDO

            DO i=1, n-middle_n-1
                   xgrid(i+middle_n)=xgrid(middle_n)+((real(i)*1.0_8)/(real(n*1.0_8-middle_n)))*(xgrid(n)-xgrid(middle_n))
            ENDDO

       ELSE
            DO i=2,n-1
                xgrid(i)=xgrid(1)+(real(i)-1.0_8)*(xgrid(n)-xgrid(1))/(real(n)-1.0_8)
            ENDDO

            middle_n=(n+1)/2
       END IF
        

        !Transition probabilities between states: gamma(j,k)

        sumgammarow=0.0_8
        DO j=1,n        !Loop over states today

            !If tomorrow's state is 1
            !ub=(((xgrid(1)+xgrid(2))/2.0_8)-rho*xgrid(j))/sigmaeps
            !gama(j,1)=ANORDF(ub)            !Evaluates normal CDF(x)
            ub=(((xgrid(1)+xgrid(2))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps)
            gama(j,1)=0.5_8*(1+DERF(ub))
           
           
            sumgammarow=gama(j,1)

            !If tomorrow's state is 2 to n-1
            DO k=2,n-1

                !ub=(((xgrid(k)+xgrid(k+1))/2.0_8)-rho*xgrid(j))/sigmaeps
                !lb=(((xgrid(k)+xgrid(k-1))/2.0_8)-rho*xgrid(j))/sigmaeps
               ub=(((xgrid(k)+xgrid(k+1))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps) 
               lb=(((xgrid(k)+xgrid(k-1))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps)
                
                !gama(j,k)=ANORDF(ub)-ANORDF(lb)
                gama(j,k)=0.5_8*((1+DERF(ub))-(1+DERF(lb)))

                sumgammarow=sumgammarow+gama(j,k)

            ENDDO

            !If tomorrow' state is n
            gama(j,n)=1.0_8-sumgammarow
            
            
            ! NOW ADJUST SO THAT THE AUTOCORRELATION IS PRECISE!!!! (the cost is that the variances could now deviate)
            !---------- this part can be commented out
            
                 
                            temp_prob=0.0_8
                            temp_ac=0.0_8
                            temp_expectation=0.0_8
                            DO k=1, n
                                IF(j .NE. k) THEN
                                    temp_prob=temp_prob+gama(j,k)
                                    temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
                                END IF
                            END DO
                            temp_expectation=temp_expectation/temp_prob
                            
                            ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
                            ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
                            
                            temp_ac=(rho*xgrid(j)-temp_expectation)/(xgrid(j)-temp_expectation)

                            !WRITE(*,*) 'j=', j, 'temp_ac=', temp_ac
                            IF(temp_ac<0.0_8 .AND. ((j .NE. middle_n) .OR. (j .NE. middle_n+1) )) THEN
                            
                                ! THIS PROCEDURE DOESN'T WORK YET!!!!!

                                !WRITE(*,*) 'ERROR IN TAUCHEN AUTOCORRELATION CORRECTION PROCEDURE'
                                !!STOP
                                
                                ! this means that instead of autoregressive, the way probability mass is distributed, it moves the expected value away from zero
                                ! solution here: shift mass to zero (center of grid) 
                                
!                                IF(j<middle_n) THEN             ! shift mass to middle_n
!                                
!                                        temp_prob=0.0_8
!                                        temp_ac=0.0_8
!                                        temp_expectation=0.0_8
!                                        DO k=1, n
!                                            IF(j .NE. middle_n) THEN 
!                                                temp_prob=temp_prob+gama(j,k)
!                                                temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
!                                            END IF
!                                        END DO
!                                        temp_expectation=temp_expectation/temp_prob
!                                        
!                                        ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
!                                        ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
!                                        
!                                        temp_ac=(rho*xgrid(middle_n)-temp_expectation)/(xgrid(middle_n)-temp_expectation)
!
!                                        IF(temp_ac<0.0_8) !STOP 'ERROR: STILL NO AUTOCORRELATION PRODUCED IN CORRECTION PROCEDURE!'
!                                        
!                                        gama(j,middle_n)=temp_ac
!                        
!
!                                        DO k=1, n
!                                             IF(j .NE. middle_n) THEN 
!                                                gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)   
!                                             END IF
!                                        
!                                        END DO
!                                       
!                                
!                                ELSE IF (j>middle_n+1) THEN         ! shift mass to middle_n +1 
!                                
!                                
!                                        temp_prob=0.0_8
!                                        temp_ac=0.0_8
!                                        temp_expectation=0.0_8
!                                        DO k=1, n
!                                            IF(j .NE. middle_n+1) THEN
!                                                temp_prob=temp_prob+gama(j,k)
!                                                temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
!                                            END IF
!                                        END DO
!                                        temp_expectation=temp_expectation/temp_prob
!                                        
!                                        ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
!                                        ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
!                                        
!                                        temp_ac=(rho*xgrid(middle_n+1)-temp_expectation)/(xgrid(middle_n+1)-temp_expectation)
!                                        
!                                        IF(temp_ac<0.0_8) !STOP 'ERROR: STILL NO AUTOCORRELATION PRODUCED IN CORRECTION PROCEDURE!' 
!
!                                        gama(j,middle_n+1)=temp_ac
!                        
!                                        
!                                        DO k=1, n
!                                             IF(j .NE. middle_n+1) THEN 
!                                                gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)   
!                                             END IF
!                                        
!                                        END DO
!
!
!
!
!                                END IF


                            ELSE IF (temp_ac .GE. 0.0_8 .AND. temp_ac<0.98_8) THEN
                                ! here property is indeed autoregressive, though either not enough, or too much:
                                ! solution:  shift mass to or fro the point of origin, j

                            gama(j,j)=temp_ac


                            DO k=1, n
                                 IF(j .NE. k) THEN
                                    gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)
                                 END IF

                            END DO


                            END IF

                            ! to be wholly on the good side of everything, we should also check whether first-order stoch dominance is preserved!!!

                    ENDDO


        !Take each log(y) state back to power e (undo the log):
        DO i=1,n
            xgrid(i)=exp(xgrid(i))
        ENDDO
    !
    !        OPEN(UNIT=20, file='tauchen.txt', form='formatted', status='replace')
    !
    !               WRITE(20,FMT='(A10)', ADVANCE='no')  'grid'
    !               DO i=1,n
    !                        WRITE(20,FMT='(I3,A6,X)', ADVANCE='no') i,'trans'
    !               END DO
    !
    !               DO i=1,n
    !                  WRITE(20,FMT='(F9.4,X)', ADVANCE='no')  xgrid(i)
    !                  DO k=1,n-1
    !                    WRITE(20,FMT='(F9.4,X)', ADVANCE='no')  gama(i,k)
    !                  END DO
    !                  WRITE(20,FMT='(F9.4,X)')  gama(i,n)
    !              END DO
    !              IF (precisionind==1) WRITE(20,*) 'middle_n=', middle_n, ' and n =', n
    !         CLOSE(20)
    !

    END SUBROUTINE sub_tauchen


   SUBROUTINE sub_tauchen_precision(n,lambda,rho,sigmaeps,xgrid,gama, precision_lb, precision_ub)



        !Dummy variables
        REAL(8), INTENT(in) :: rho, sigmaeps    !AR persistence param. and stdev of AR error term
        INTEGER, INTENT(in) :: n
        REAL(8), INTENT(in) :: precision_lb, precision_ub
        REAL(8), INTENT(in) :: lambda        !Number of states in Markov chain; no of sd's to top state
        INTEGER :: i, j, k

        !Local variables
        REAL(8) :: stdevx, ub, lb, sumgammarow    !Unconditional stdev of the variable; intermediates
        INTEGER :: middle_n, n_precision_lb, n_precision_ub
        
        !Output
        REAL(8), DIMENSION(n), INTENT(out) :: xgrid        !Grid of shock states
        REAL(8), DIMENSION(n,n), INTENT(out) :: gama    !Resulting transition matrix
        
        REAL(8) :: temp_prob, temp_expectation, temp_ac

        !Make vector of discrete states: y_N=lambda*(uncond'l st. dev. of the variable)

        
        stdevx=sigmaeps/((1.0_8-rho**2.0_8)**(0.5_8))

        xgrid(1) = -stdevx*lambda
        xgrid(n) = stdevx*lambda
         
        ! POPULATE THE X-GRID
        
            IF (n<12) THEN
                WRITE(*,*) 'too little gridpoints for precision, paused in sub_tauchen_precision'
                !STOP 
            END IF 
             
            
            n_precision_lb=n/4
            n_precision_ub=(3*n)/4
            
            xgrid(n_precision_lb)=LOG(precision_lb)
            xgrid(n_precision_ub)=LOG(precision_ub)
            
            WRITE(*,*) 'precision_lb= ', precision_lb
            WRITE(*,*) 'precision_ub= ', precision_ub
                
            DO i=2,n_precision_lb-1
                xgrid(i)=xgrid(1)+(real(i)-1.0_8)*(xgrid(n_precision_lb)-xgrid(1))/(real(n_precision_lb)-1.0_8)
                WRITE(*,*) 'TAUCHEN_CTV: xgrid(i)= ', xgrid(i)
            ENDDO
            
            

            DO i=n_precision_lb+1,n_precision_ub-1
                xgrid(i)=xgrid(n_precision_lb)+(real(i)-real(n_precision_lb))*(xgrid(n_precision_ub)-xgrid(n_precision_lb))/(real(n_precision_ub)-real(n_precision_lb))
                WRITE(*,*) 'xgrid(i)= ', xgrid(i)
            ENDDO
       
            DO i=n_precision_ub+1,n
                xgrid(i)=xgrid(n_precision_ub)+(real(i)-real(n_precision_ub))*(xgrid(n)-xgrid(n_precision_ub))/(real(n)-real(n_precision_ub))
                WRITE(*,*) 'xgrid(i)= ', xgrid(i)
            ENDDO
        
        !Transition probabilities between states: gamma(j,k)

        sumgammarow=0.0_8
        DO j=1,n        !Loop over states today

            !If tomorrow's state is 1
            !ub=(((xgrid(1)+xgrid(2))/2.0_8)-rho*xgrid(j))/sigmaeps
            !gama(j,1)=ANORDF(ub)            !Evaluates normal CDF(x)
            ub=(((xgrid(1)+xgrid(2))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps)
            gama(j,1)=0.5_8*(1+DERF(ub))
           
           
            sumgammarow=gama(j,1)

            !If tomorrow's state is 2 to n-1
            DO k=2,n-1

                !ub=(((xgrid(k)+xgrid(k+1))/2.0_8)-rho*xgrid(j))/sigmaeps
                !lb=(((xgrid(k)+xgrid(k-1))/2.0_8)-rho*xgrid(j))/sigmaeps
               ub=(((xgrid(k)+xgrid(k+1))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps) 
               lb=(((xgrid(k)+xgrid(k-1))/2.0_8)-rho*xgrid(j))/(sqrt(2.0_8)*sigmaeps)
                
                !gama(j,k)=ANORDF(ub)-ANORDF(lb)
                gama(j,k)=0.5_8*((1+DERF(ub))-(1+DERF(lb))) 

                sumgammarow=sumgammarow+gama(j,k)

            ENDDO
           
            !If tomorrow' state is n
            gama(j,n)=1.0_8-sumgammarow
            
            
            ! NOW ADJUST SO THAT THE AUTOCORRELATION IS PRECISE!!!! (the cost is that the variances could now deviate)
            !---------- this part can be commented out
            
                 
                            temp_prob=0.0_8
                            temp_ac=0.0_8
                            temp_expectation=0.0_8
                            DO k=1, n
                                IF(j .NE. k) THEN
                                    temp_prob=temp_prob+gama(j,k)
                                    temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
                                END IF
                            END DO
                            temp_expectation=temp_expectation/temp_prob

                            ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
                            ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
                            
                            temp_ac=(rho*xgrid(j)-temp_expectation)/(xgrid(j)-temp_expectation)
                            
                            !WRITE(*,*) 'j=', j, 'temp_ac=', temp_ac
                            IF(temp_ac<0.0_8 .AND. ((j .NE. middle_n) .OR. (j .NE. middle_n+1) )) THEN
                            
                                ! THIS PROCEDURE DOESN'T WORK YET!!!!!
                                
                                !WRITE(*,*) 'ERROR IN TAUCHEN AUTOCORRELATION CORRECTION PROCEDURE'
                                !!STOP
                                
                                ! this means that instead of autoregressive, the way probability mass is distributed, it moves the expected value away from zero
                                ! solution here: shift mass to zero (center of grid)
                                
!                                IF(j<middle_n) THEN             ! shift mass to middle_n
!                                
!                                        temp_prob=0.0_8
!                                        temp_ac=0.0_8
!                                        temp_expectation=0.0_8
!                                        DO k=1, n
!                                            IF(j .NE. middle_n) THEN 
!                                                temp_prob=temp_prob+gama(j,k)
!                                                temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
!                                            END IF
!                                        END DO
!                                        temp_expectation=temp_expectation/temp_prob
!
!                                        ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
!                                        ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
!                                        
!                                        temp_ac=(rho*xgrid(middle_n)-temp_expectation)/(xgrid(middle_n)-temp_expectation)
!                                        
!                                        IF(temp_ac<0.0_8) PAUSE 'ERROR: STILL NO AUTOCORRELATION PRODUCED IN CORRECTION PROCEDURE!' 
!                                        
!                                        gama(j,middle_n)=temp_ac
!                        
!                                        
!                                        DO k=1, n
!                                             IF(j .NE. middle_n) THEN 
!                                                gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)   
!                                             END IF
!                                        
!                                        END DO
!                                       
!                                
!                                ELSE IF (j>middle_n+1) THEN         ! shift mass to middle_n +1
!                                
!                                
!                                        temp_prob=0.0_8
!                                        temp_ac=0.0_8
!                                        temp_expectation=0.0_8
!                                        DO k=1, n
!                                            IF(j .NE. middle_n+1) THEN
!                                                temp_prob=temp_prob+gama(j,k)
!                                                temp_expectation=temp_expectation+(gama(j,k)*xgrid(k))
!                                            END IF
!                                        END DO
!                                        temp_expectation=temp_expectation/temp_prob
!                                        
!                                        ! if rho is the autocorrelation, x0 initial x, x1 tomorrows x, it has to be true that rho x0=alpha x0 + (1-alpha) temp_expectation
!                                        ! therefore: alpha= (rho x0-temp_expectation)/(x0-temp_expectation)
!                                        
!                                        temp_ac=(rho*xgrid(middle_n+1)-temp_expectation)/(xgrid(middle_n+1)-temp_expectation)
!                                        
!                                        IF(temp_ac<0.0_8) !STOP 'ERROR: STILL NO AUTOCORRELATION PRODUCED IN CORRECTION PROCEDURE!' 
!                                        
!                                        gama(j,middle_n+1)=temp_ac
!
!                                        
!                                        DO k=1, n
!                                             IF(j .NE. middle_n+1) THEN 
!                                                gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)   
!                                             END IF
!                                        
!                                        END DO
!                                       
!                                
!                                
!
!                                END IF
                                
                                
                            ELSE IF (temp_ac .GE. 0.0_8 .AND. temp_ac<0.98_8) THEN 
                                ! here property is indeed autoregressive, though either not enough, or too much:
                                ! solution:  shift mass to or fro the point of origin, j
                            
                            gama(j,j)=temp_ac
                            
                            
                            DO k=1, n
                                 IF(j .NE. k) THEN 
                                    gama(j,k)= ((1-temp_ac)/temp_prob)*gama(j,k)   
                                 END IF
                            
                            END DO
                                                
                            
                            END IF
                            
                            ! to be wholly on the good side of everything, we should also check whether first-order stoch dominance is preserved!!!                    

                    ENDDO

        
        !Take each log(y) state back to power e (undo the log):
        DO i=1,n
            xgrid(i)=exp(xgrid(i))
        ENDDO
    !
    !        OPEN(UNIT=20, file='tauchen.txt', form='formatted', status='replace')
    !        
    !               WRITE(20,FMT='(A10)', ADVANCE='no')  'grid'
    !               DO i=1,n 
    !                        WRITE(20,FMT='(I3,A6,X)', ADVANCE='no') i,'trans'
    !               END DO
    !               
    !               DO i=1,n
    !                  WRITE(20,FMT='(F9.4,X)', ADVANCE='no')  xgrid(i)                       
    !                  DO k=1,n-1
    !                    WRITE(20,FMT='(F9.4,X)', ADVANCE='no')  gama(i,k)
    !                  END DO
    !                  WRITE(20,FMT='(F9.4,X)')  gama(i,n)
    !              END DO
    !              IF (precisionind==1) WRITE(20,*) 'middle_n=', middle_n, ' and n =', n
    !         CLOSE(20)              
    !                                                             

    END SUBROUTINE sub_tauchen_precision
   
   
    SUBROUTINE sub_gamastar(gama,gstar)

        REAL(8), DIMENSION(:,:), INTENT(in) :: gama            !Distribution matrix of interest
        REAL(8), DIMENSION(:), INTENT(out) :: gstar            !Output: invariant distribution

        REAL(8), DIMENSION(SIZE(gstar)) :: g0                !Initial guess for invariant dist
        REAL(8) :: dist, tempreal
        INTEGER :: imax, i, sizeg,j,k

        !Initialize gstar
        sizeg=SIZE(gama)
        !WRITE(*,*) 'size gama=', sizeg
        sizeg=SIZE(gstar)
        !WRITE(*,*) 'size gstar=', sizeg

        DO i=1,size(gstar)-1
            gstar(i)=1.0_8/size(gstar)
        ENDDO

        gstar(size(gstar))=1.0_8-sum(gstar(1:(size(gstar)-1)))                    !To make sure the array adds to exactly 1


        !WRITE(*,*) 'inside stat dist tauchen'

        imax=5000
       
        i=1
        dist=10000
        
        
       
        DO WHILE ((i<imax).AND.(dist>0.0000000000001_8))

            g0=gstar
           
            

            !gstar=MATMUL(g0,gama)   !! STUPID, THIS TRIGGERS A BREAK-DOWN, no idea what is going on
            DO j=1, sizeg
                tempreal=0.0_8
                DO k=1, sizeg
                    tempreal=tempreal+g0(k)*gama(k,j)
                END DO                     
                gstar(j)=tempreal
            END DO                 
            
            dist=maxval(abs(gstar-g0))

            i=i+1

        ENDDO
       
        gstar(size(gstar))=1.0_8-sum(gstar(1:(size(gstar)-1)))
   
    END SUBROUTINE sub_gamastar
   
     
      
END MODULE mod_tauchen_ctv


!!************************************************************
!!  ROUWENHORST METHOD!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!************************************************************
!
!
!module constants
!    implicit none
!    save
!
!    integer, parameter:: dbl = kind(1d0)
!    real(dbl), parameter:: pi = 3.14159265358979d0
!    character(1), parameter:: tab = char(9)
!    character(1), parameter:: ret = char(13)
!
!end module constants
!
!module temps
!    use constants
!    implicit none
!    save
!
!    integer N2
!    real(dbl) e1, e2, sigma_z, mu_z, rho2, sigma_eps2
!end module temps
!
!module mathutils
!    use constants
!
!    implicit none
!    interface kron
!        module procedure kronMat, kronVect
!    end interface
!
!    contains
!
!    !kronecker product two matrices
!    function kronMat(Amat,Bmat)
!        implicit none
!        real(dbl), intent(in):: Amat(:,:), Bmat(:,:)
!        real(dbl) kronMat(size(Amat,1)*size(Bmat,1),size(Amat,2)*size(Bmat,2))
!        integer nA, mA, nB, mB, i, j
!
!        nA = size(Amat,1)
!        mA = size(Amat,2)
!        nB = size(Bmat,1)
!        mB = size(Bmat,2)
!        do i=1,nA
!            do j=1,mA
!                kronMat(1+(i-1)*nB:nB+(i-1)*nB,1+(j-1)*mB:mB+(j-1)*mB) = Bmat*Amat(i,j)
!            end do
!        end do
!    end function kronMat
!
!    !kronecker product of two vectors
!    function kronVect(Avect,Bvect)
!        implicit none
!        real(dbl), intent(in):: Avect(:), Bvect(:)
!        real(dbl) kronVect(size(Avect,1)*size(Bvect,1))
!        integer nA, nB, i, j
!
!        nA = size(Avect,1)
!        nB = size(Bvect,1)
!
!        do i=1,nA
!                kronVect(1+(i-1)*nB:nB+(i-1)*nB) = Bvect*Avect(i)
!        end do
!    end function kronVect
!
!    !linspace
!    function linspace(xmin,xmax,n)
!        real(dbl) xmin, xmax, linspace(n)
!        integer n, i
!
!        linspace = (/ (xmin + (xmax-xmin)*i/(n-1),i=0,n-1) /)
!    end function linspace
!end module mathutils
!
!
!MODULE discretizeAR1
!    use constants
!    implicit none
!
!    CONTAINS
!
!
!subroutine Rouwenhorst(rho, mu_eps, sigma_eps, N, zvect, Pmat)
!        use mathutils
!        implicit none
!
!        real(dbl), intent(in):: rho, mu_eps, sigma_eps
!        integer, intent(in):: N
!        real(dbl), intent(out):: zvect(N)
!        real(dbl), intent(out):: Pmat(N,N)
!
!        real(dbl) mu_z, sigma_z, q, eps
!        real(dbl), allocatable, dimension(:,:):: P1, P2
!        integer status, i, j
!        integer, parameter :: lognormal_ind=1
!
!
!        mu_z = mu_eps/(1-rho)
!        sigma_z = sigma_eps/(sqrt(1-rho**2))
!
!        q = (rho+1)/2
!        eps = sqrt(dble(N-1)) * sigma_z
!
!        if (N == 1) then
!            Pmat = 1.0d0
!            zvect = mu_z
!            return
!        else if (N == 2) then
!            Pmat = reshape((/q, 1-q, 1-q, q/),(/2,2/))
!            zvect = (/mu_z-eps,mu_z+eps/)
!            return
!        end if
!
!        allocate(P1(2,2),stat=status)
!        P1 = reshape((/q, 1-q, 1-q, q/),(/2,2/))
!
!        do i=2,N-1
!            allocate(P2(i+1,i+1),stat=status)
!            P2 = q * reshape( (/  (/(P1(:,j),0.0d0 ,j=1,i)/) ,  (/(0.0d0,j=1,i+1)/)    /), (/i+1,i+1/) ) + &
!                 (1-q) * reshape( (/  (/(0.0d0,j=1,i+1)/), (/ (P1(:,j),0.0d0 ,j=1,i)/)   /) ,   (/i+1,i+1/) ) + &
!                 (1-q) * reshape( (/  (/ (0.0d0,P1(:,j) ,j=1,i) /) ,  (/(0.0d0,j=1,i+1)/)  /), (/i+1,i+1/) ) + &
!                 q * reshape( (/ (/(0.0d0,j=1,i+1)/), (/(0.0d0,P1(:,j) ,j=1,i)/)   /) ,   (/i+1,i+1/) )
!
!            P2(2:i,:) = P2(2:i,:)/2
!
!            deallocate(P1,stat=status)
!
!            if (i==N-1) then
!                Pmat = P2
!            else
!                allocate(P1(i+1,i+1), stat=status)
!                P1 = P2
!            end if
!
!            deallocate(P2,stat=status)
!        end do
!
!        zvect = linspace(mu_z-eps,mu_z+eps,N)
!
!
!
!        !Take each log(y) state back to power e (undo the log):
!        IF (mu_eps==0 .AND. lognormal_ind==1) THEN
!        DO i=1,N
!            zvect(i)=exp(zvect(i))
!        END DO
!        END IF
!
!end subroutine Rouwenhorst
!end module discretizeAR1
