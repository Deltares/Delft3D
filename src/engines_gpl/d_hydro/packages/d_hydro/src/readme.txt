Issue 15009, 14967:

On Linux with dynamically loaded Flow2D3D, D_Hydro.exe crashes
sometimes when using domain decomposition.
Often a stack trace is shown in the output indicating the problem
occurs in a simple FORTRAN read/write statement.

This problem seems to have been introduced in Delft3D-FLOW version 4.00.00.00.
This was the first open source version, and the first to use dynamic loading.
There is no solution yet.

A workaround is to link everything together in a single, monolithic executable.

Steps:

1.  Go to directory .../src/engines_gpl/d_hydro/packages/d_hydro/src
2.  Switch make files:
        cp -f Makefile_monolithic.am Makefile.am 
3.  Build and run as usual

To restore:

1.  Go to directory .../src/engines_gpl/d_hydro/packages/d_hydro/src
2.  Switch make files:
        cp -f Makefile_dynamic.am Makefile.am 

----
Adri.Mourits@Deltares.NL
Irv.Elshoff@Deltares.NL
29 oct 11
