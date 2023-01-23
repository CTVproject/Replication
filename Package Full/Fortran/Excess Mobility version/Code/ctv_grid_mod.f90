MODULE ctv_grid_mod

IMPLICIT NONE

!--------------------------
! PRODUCTIVITY GRID
!---------------------------

!---------------------------
!   OCCUPATION-WORKER PRODUCTIVITY
INTEGER, PARAMETER 	:: zpts=120 !240 !300 !120     !50        ! basic grid (later made smaller, if without loss of results)
                                ! BASELINE USED IN THE ESTIMATION 120
!---------------------------
!   HUMAN CAPITAL PRODUCTIVITY
INTEGER, PARAMETER 	:: xpts=3               
!---------------------------
!   AGGREGATE PRODUCTIVITY
INTEGER, PARAMETER 	:: ppts=20 !30 !40 !20   ! basic grid for aggregate shocks
                                ! BASELINE USED IN THE ESTIMATION 20

INTEGER			:: estim_counter = 0
REAL(8)			:: estim_best = huge(1.0_8)

END MODULE ctv_grid_mod
