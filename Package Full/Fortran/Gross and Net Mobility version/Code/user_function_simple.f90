
    
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
    
    
    
    
    PROGRAM ctv_grossnetmob

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
      INTEGER :: mpthread, reasonflag, readflag, file_exists

        REAL(8),DIMENSION(50,1) :: vars=0.5
        INTEGER,PARAMETER :: nmom_local=116
        INTEGER, PARAMETER      :: nparl=36
        REAL(8), DIMENSION(36) :: theta, theta_read               !  Vector of moments put into the model. This is after rescaling, taking the bounds into account
        REAL(8), DIMENSION(36) :: rescale_vector       !  Vector for rescaling the bounds

        REAL(8), DIMENSION(nmom_local) :: mom                    !

    ! SWITCHES
        INTEGER, PARAMETER :: readdata_switch=0
        INTEGER, PARAMETER :: read_bounddata_switch=0

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

        INTEGER, PARAMETER :: netmob_parameters_ind=0
        INTEGER, PARAMETER :: netmob_22par_ind=1
        INTEGER, PARAMETER :: par28netmob_ind=1
        INTEGER, PARAMETER :: par36netmob_ind=0
        
! THREAD NUMBER
!mpthread=MY_RANK


theta=0.0_8


IF (test_general_ind==1) THEN

    

!! v30r4 200418

 !7.603851   :c  124.834873   :k    0.829931   :b    0.239473  :eta
 !  0.003467:delta   0.003467:d_lhc   0.000239:d_hhc   0.353748:zcor 
 !   0.99852:rhop     0.00201:sigp     0.99831:rhoz     0.00716:sigze    0.12311:sigFz
 !  1.170681  :x5    1.458027 :x10    0.003162:skdep
!
!     --- NET MOBILITY PARAMETERS ---
!  occprod: *   1.0185  , (implied)   0.9877  , *   1.0035  , *   0.9876
!  occelas: *   1.0814  , (implied)   1.1200  , *   0.5318  , *   1.2829
!  ocentry: *   0.6203  , *           0.7654  , *   0.8526  , i   1.0000
!  ocenpdf: *   0.6203  , *           0.1451  , *   0.0872  , i   0.1474
!cr_occbegdst   0.2236  ,             0.2924  ,     0.2263  ,     0.2577
!   occbegdst   0.2224  ,             0.2984  ,     0.2269  ,     0.2523
!  Occupation search technology parameters
!      o1s: (share)   0.4360      , *    0.5603       , *   0.0038  , (imp)    0.0000
!            o2s: *   0.4068  , (shr)    0.3831       , *   0.2100  , (imp)    0.0000
!            o3s: *   0.0001      , *    0.0928  , (shr)    0.3835  , (imp)    0.5236
!            o4s: *   0.0001      , *    0.1397  , (imp)    0.7667  , (shr)    0.0935
!  Occupation search elasticity parameter:   3.589031774838692E-002
!  
!     0.00347	0.00347	0.00024	7.60385	124.83487	0.82993	0.99852	0.00201	0.99831	0.00716	0.23947	0.35375	1.17068	1.45803	0.00316	1.01849	0.98769	1.00348	0.98755	1.08144	1.11997	0.5318	1.28291	0.6203	0.7654	0.8526	1	0.43595	0.56026	0.00379	0	0.40684	0.38314	0.21002	0	0.00006	0.09282	0.38348	0.52363	0.00009	0.13971	0.76668	0.09352	0.03589

     theta= (/ &
  3.467014339731860E-003 ,&
   7.60385078038352     ,&
   124.834873400718     ,&
  0.829931343360671     ,&
  0.998521090007912     ,&
  2.006003951423729E-003,& !  2.012E-003,&             ! trying to increase volatility
  0.998306610379322     ,&
  7.161753177825330E-003,&
  0.239473394632211     ,&
  0.353747705119405     ,&
   1.17068089297431     ,&
   1.45802664632586     ,&
  3.162244680989140E-003,&
  2.390310495102519E-004,&
   1.01848640114317     ,&
   1.00348440117175     ,&
  0.987552151075010     ,&
   1.08143842370888     ,&
  0.531798318952403     ,&
   1.28290698975068     ,&
  0.620335815608801     ,&
  0.145081055419019     ,&
  8.718516004635174E-002,&
  0.435950169337722     ,&
  0.993287658353076     ,&
  6.711328367963287E-003,&
  0.659531253368415     ,&
  0.653127229361175     ,&
  9.999999747378752E-005,&
  0.150558113213578     ,&
  9.999999747378752E-005,&
  0.154119874382047     ,&
  3.589031774838692E-002,&
  0.383136731203937     ,&
  0.383483418133582     ,&
  9.352119398612471E-002 /)

    
     
END IF
  


!!====================================================
!! READ PARAMETERS FOR RUN
!!=====================================================

!! read best parameters of the previous run
        IF(readdata_switch==1) THEN
            readflag=0
            INQUIRE(FILE="bestpar_foruse.txt", EXIST=file_exists)

            IF (file_exists .AND. readdata_switch==1) THEN
                WRITE(*,*) 'reading data -- single read'
                OPEN(unit=91, file="bestpar_foruse.txt")
                readflag=1
                READ(91,*) npar_check
                IF(npar_check .ne. nparl) THEN
                    WRITE(*,*) 'no. pars set .ne. no. pars in file'
                    readflag=0
                ELSE
                    DO counter1=1, nparl
                        READ(91,*, IOSTAT=reasonflag) theta_read(counter1)
                        WRITE(*,*) 'par: ', counter1, ', value : ', theta_read(counter1)
                        IF(reasonflag .ne. 0) readflag=0
                    END DO
                END IF
                CLOSE(91)
            ELSE
                WRITE(*,*) 'PROBLEM LOCATING BESTPAR_FORUSE.TXT, continuing with pars from program'
            END IF

            IF (readflag==1) THEN
                WRITE(*,*) 'overwriting program pars with read-in pars'
                theta=theta_read
            END IF
        END IF



mpthread=0
WRITE(*,*) 'entering program' 
Call CTV_function_simple(theta,mom,tempreal, mpthread)

WRITE(*,*) tempreal

    END PROGRAM ctv_grossnetmob




