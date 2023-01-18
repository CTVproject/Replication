# Replication

Unpack replication.zip first. There is a subdirectory Fortran in the unpacked folder. Unpack the two remaining zip files  (CTV_Fortran_1.zip and CTV_Fortran_2.zip) inside the Fortran folder. 

More information is in the READMEs of the package, but the order of replication is: 
1) Run Fortran programs
2) Set the workingdir global in global_paths.do to the root of the replication folder, where CTV_master.do is.
3) Run CTV_master.do. 

With the correct location of the fortran programs, STATA will use and move the Fortran results to the appropriate 'results' location. 
