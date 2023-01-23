

In this directory is 
    - the .do file that produces the transition matrix used to calculate the miscoding matrix
    - this transition matrix is saved in two places: 
                    1. gamma_out_c.xlsx
                    2. occgamma_mm_c_v22.xlsx, sheet: 'transmat_cnormssym'
    - there are two ways of calculating the miscoding matrix
                    1. Mathematica: measurement error correction_22occ.nb
                    2. Matlab: Gamma_manip.m 
    - these two methods produce a matrix saved in the following files, resp.
                    1. Ginv_mm_mathe.xlsx (produced by mathematica)
                    2. Gammainv_mm_c_v2.xlsx (produced by matlab)
    - for comparison purposes, we have also included the two files that contain the matrix used to correct mobility in the paper
                    1. Ginv_mm_mathe_REFERENCE.xlsx
                    2. Gammainv_mm_c_v2_REFERENCE.xlsx
     Thus: Ginv_mm_mathe.xlsx and Ginv_mm_mathe_REFERENCE.xlsx should be identical (unless the user made it their own changes, of course). Same for Gammainv_mm_c_v2 and Gammainv_mm_c_v2_REFERENCE. 

    - Gamma_inv matrix is  read back into stata in the Ginv_matrices.do, which is also in this directory 
        (and contains the older miscoding matrices for other classifications, etc. as well )

    Thus, to incorporate a freshly calculated miscoding matrix into the stata do-files, one needs to
    run either the matlab or mathematica notebook between step 1 and step 2  of the STATA do file sequence.
    
    To verify correctnesso of the garbling matrix ex post, one can also simply check, ex post, that the files coincides with their corresponding *_REFERENCE.xlsx versions.

    