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

      PROGRAM ctv_test
      
      !USE cmaes_param_mod
      USE mod_solve_ctv
      
      !USE cmaes_param_mod
      !USE ctv_grid_mod
      !USE ctv_grid_mod_VE_VU
      !USE CTV_wrapper

      
      !-------------------------------------------------------------------------
      !  Parameters
      !-------------------------------------------------------------------------
      !REAL(MK),DIMENSION(n)                        :: res
      !REAL(MK),DIMENSION(m,n)                       :: vars
      !INTEGER,INTENT(in)                           :: m
      !INTEGER,INTENT(in)                           :: n
      !REAL(MK),DIMENSION(m),OPTIONAL              :: lbounds
      !REAL(MK),DIMENSION(m),OPTIONAL              :: ubounds
      REAL(8) ::  tempreal
      INTEGER :: mpthread
    
      
        INTEGER,PARAMETER :: nmom_local=56
        REAL(8), DIMENSION(14) :: theta                !  Vector of moments put into the model. This is after rescaling, taking the bounds into account
        REAL(8), DIMENSION(14) :: rescale_vector       !  Vector for rescaling the bounds
    
        REAL(8), DIMENSION(nmom_local) :: mom                    !
    
    ! SWITCHES
        INTEGER, PARAMETER :: read_bounddata_switch=1
    
    ! TO CHANGE THE VERSION OF THE MODEL    
    ! 1) ADJUST NuMBER OF PARAMETERS ALSO IN THE PARAMETER .TXT FILE
    ! 2) ADJUST THE PARAMETER DEF/RANGES IN USER_FUNCTION
    ! 3) ADJUST HOW THE PROGRAM IS RUN IN MODGLOBAL_CTV_EFF.f90 and if necessary in mod_solve_Ctv.f90
        
        !INTEGER, PARAMETER :: lifecycle_uf_ind=1            ! OTHER LIFE CYCLE INDICATOR IN MODGLOBAL_CTV_EFF.f90. HERE RuN SMALLER-SET-PARAMETER VERSION (no need for x2, x3, skilldep)
        INTEGER, PARAMETER :: par13skilldep_ind=0
        INTEGER, PARAMETER :: par12noskilldep_ind=0
        INTEGER, PARAMETER :: full_hcdep_version_ind=0
        INTEGER, PARAMETER :: full_hcdep_sepheterogeneity_ind=0
        iNTEGER, PARAMETER :: par9nohc_ind=0
        iNTEGER, PARAMETER :: par7nohcnos_ind=0
        iNTEGER, PARAMETER :: par6nohcnos_ind=0             ! 
        INTEGER, PARAMETER :: nosearch_noreall_ind=0        ! smaller set of parameters
        INTEGER, PARAMETER :: nosearch_reall_ind=0          ! smaller set of parameters
        INTEGER, PARAMETER :: test_ind=0
        INTEGER, PARAMETER :: test_hc_ind=0
        INTEGER, PARAMETER :: test_6par_ind=0
        INTEGER, PARAMETER :: test_general_ind=1            ! at the bottom of this code, playground for parameter values, just before calling the main program
        
        
! THREAD NUMBER        
!mpthread=MY_RANK


theta=0.0_8


!    
!!===========================================
!!  RUN !
!!==============================================

IF (test_general_ind==1) THEN 


! 
!  theta(1)=5.431461766710610E-003
!  theta(2)=HUGE(1.0)
!  theta(3)=404.729268017385     
!  theta(4)=0.697341634195411     
!  theta(5)=0.998301704060974     
!  theta(6)=2.152334872753163E-003
!  theta(7)=0.994263297715495     
!  theta(8)=2.218586232302374E-002
!  theta(9)=0.235158279402192     
!  theta(10)=0.205497125757748     
!  theta(11)=1.25695845109192     
!  theta(12)=1.30548304418967     
!  theta(13)=6.922340182643621E-003
!  theta(14)=1.180627831509296E-003

!!!  V16r2 NO REALL (PLAMBDA=3.0; tmax_sim=20000)
 
  theta(1)=4.735180213201740E-003
  theta(2)=HUGE(1.0)
  theta(3)=102.018667507909     
  theta(4)=0.839879496371608     
  theta(5)=0.996653482133555     
  theta(6)=1.769287913210786E-003
  theta(7)=0.992813386257737     
  theta(8)=1.320066052273215E-002
  theta(9)=0.244807013921834     
  theta(10)=0.201228292479774     
  theta(11)=1.27205501833211     
  theta(12)=1.30223970332199     
  theta(13)=4.251682289483694E-003
  theta(14)=1.553664809283650E-003
 
!! EXCESS MOBILITY MODEL (plambda=2.0 tmax=40000)  v25r1 CALIBRATION
  !theta(1)=3.363815357989426E-003
  !theta(2)= 7.54898893359425
  !theta(3)=125.732826596031
  !theta(4)=0.843299677677423
  !theta(5)=0.998446087494502
  !theta(6)=1.978149808922647E-003
  !theta(7)=0.998227586913318
  !theta(8)=7.074988839335767E-003
  !theta(9)=0.240893626892578
  !theta(10)=0.348722791505610
  !theta(11)=1.18053181374619
  !theta(12)=1.47437312935831
  !theta(13)=3.869480969692593E-003
  !theta(14)=3.540999329042808E-004

  
  
END IF 


mpthread=0
Call CTV_function_simple(theta,mom,tempreal, mpthread)  
    WRITE(*,*) 'DONE WITH PROGRAM'
    print *, char(7)
      
END PROGRAM ctv_test
