These testcases are copied from: https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/12_dflowfm/test_data

Each testcase contains:
1. run.bat            file to run on Windows                   typically containing one line, referring to a run script belonging to a set of binaries.
2. run.sh             file to run on Linux                     typically containing one line, referring to a run script belonging to a set of binaries.
3. run_native_h7.sh (Copy from https://git.deltares.nl/oss/delft3d/-/tree/main/src/scripts_lgpl/singularity) file to run on the Deltares h7 cluster. (For dwaq testcases, the path to dwaq libraries should be added to the script.)
4. submit_singularity_h7.sh (Copy from https://git.deltares.nl/oss/delft3d/-/tree/main/src/scripts_lgpl/singularity) file to run an Apptainer on the Deltares h7 cluster. (For dwaq testcases, the path to dwaq libraries should be added to the script.)
5. dimr_config.xml input file for DIMR. It mainly contains a reference to the mdu/mdw-file. For parallel D-Flow FM computations: the "<process>" and "<mpiCommunicator>" lines are important.


To run your own model:
1. Copy the relevant one-line-script mentioned above into your working directory
   These scripts refer to version "latest". You can replace this to have it referred to a specific version.
   Example: p:\d-hydro\dimrset\weekly\2.09.05_63956\x64\bin\run_dimr.bat
2. Copy the relevant "dimr_config.xml" into your working directory
   Open it in an editor and change the name of the mdu/mdw-file it refers to.
3. Execute the one-line-script (without qsub)



adri.mourits@deltares.nl
yvonne.olij@deltares.nl