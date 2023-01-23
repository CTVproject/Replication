	  !-------------------------------------------------------------------------
      !  MODULE   :          ctv_grid
      !-------------------------------------------------------------------------
      !
      !  Purpose      : This tries to introduce VE, VU, which are shared between iterations
      !                
      !  Remarks      : attempt!
      !
      !  References   : 
      !
      !  Revisions    :
      !-------------------------------------------------------------------------
      !-------------------------------------------------------------------------
      !   For use in CMAES-CTV
      !   
      !-------------------------------------------------------------------------

    
    MODULE ctv_grid_mod_VE_VU
    
    USE ctv_grid_mod
    
    IMPLICIT NONE
    
    

!----------------------------
!  VE, VU
!-----------------------------

REAL(8), DIMENSION(ppts, xpts, zpts) :: VE
REAL(8), DIMENSION(ppts, xpts, zpts) :: VU
    
    
    END MODULE ctv_grid_mod_VE_VU