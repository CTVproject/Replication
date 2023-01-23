

    MODULE ctv_grid_mod_VE_VU

    USE ctv_grid_mod

    IMPLICIT NONE



!----------------------------
!  VE, VU
!-----------------------------

REAL(8), DIMENSION(ppts, xpts, zpts) :: VE
REAL(8), DIMENSION(ppts, xpts, zpts) :: VU

!----------------------------
!  VE, VU OCCUPATION WIDE
!-----------------------------

REAL(8), DIMENSION(occpts, ppts, xpts, zpts) :: VEO
REAL(8), DIMENSION(occpts, ppts, xpts, zpts) :: VUO


    END MODULE ctv_grid_mod_VE_VU
