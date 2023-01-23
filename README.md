# Replication

(c) CTV, January 2023 




## I. The Replication Package in 3 Zip Files for quicker download

We uploaded a zipped version in three parts (the replication package contains many files, especially to read in each wave of the SIPP). Unpack replication.zip first. There is a subdirectory Fortran in the unpacked folder. Unpack the two remaining zip files  (CTV_Fortran_1.zip and CTV_Fortran_2.zip) inside the Fortran folder, so that directory structure is e.g. `./Replication/Fortran/Excess Mobility version/.`  

   
## II. The Full Replication Package (not zipped)    

In the `Package Full` directory, we have put the entire replication such that it is ready to run. 

## III. Running the Replication

Detailed information is in the READMEs of the package, but the order of replication is: 

(1) Run the various Fortran programs.

(2) Set the `workingdir` global in global_paths.do to the root of the replication folder, where CTV_master.do is.

(3) Run CTV_master.do.  
    -  With the correct location of the fortran programs, STATA will use and move the Fortran results of (1) to the appropriate 'results' location, with a (hopefully) self-explanatory name (like appxtablex_model.txt, for the model-generated statistics in Table X of the online appendix). 


### IV. Tramo-Seats, also included in this replication package

The Replication Package uses TRAMO-SEATS to deal with timeseries with (time) gaps. Specifically, we use the TSW+ program to apply TRAMO-SEATS. This program can be installed using `TSW+ x64 .msi` in the TRAMO-SEATS directory here. 
