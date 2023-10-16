# DFLOWFM_NOAA_BMI
DFLOWFM source code modified for BMI compliancy as well as repository for DFLOWFM BMI

Author: Jason Ducker, Lynker Technologies LLC., Affiliate for Office of Water Prediction at NOAA

# Requirements for compiling DFLOWFM and BMI libraries, currently setup for the RDHPCS NOAA Hera cluster
# to be correctly compiled and linked to BMI shared libraries
1. cmake >= 3.20
2. szip >= 2.1
3. intel >= 2022.3.0
4. impi >= 2022.3.0
5. netcdf >= 4.7.0
6. proj >= 7.1.0
7. gdal
8. petsc >= 3.19
9. metis >= 5.1

# DFlowFM Documentation and Model Setup
For infromation about DFlowFM model engine physics, file descriptions, and model setups for a given simulation; please refer to Deltares reference manual link below:
https://content.oss.deltares.nl/delft3d/D-Flow_FM_User_Manual.pdf

# Top directory structure
src/ = DFLOWFM model engine source code 
build.sh = Cmake compilation and build script for DFLOWFM source code
BMI_build/ = Directory where BMI source code, BMI driver cmake build, and BMI driver source code is located for current DFLOWFM BMI build. 

# BMI directory structure
BMI/ = Directory where BMI shared library source code is located
src/ = Directory where BMI driver source code is located
dflowfm_install = Directory where the user will place DFLOWFM model engine compiled libraries and modules required to link and build BMI shared libraries and driver
iso_c_fortran_bmi = Directory where BMI Fortran source code is linked for compiling C+ pointer functions within BMI shared library.
CMakeLists.txt, namelist.input = Files for compiling DFLOWFM BMI shared libraries and driver using CMake and the required "BMI config" file for DFLOWFM respectively.
dflowfm_source.sh = shell script for loading and linking DFLOWFM required libraries to build model source code and the BMI shared libraries.

# Steps below to compile DFlowFM model source code
1. Unpack DFlowFM source code. Go into directory and open the build.sh file. Go to lines 190-192 and change the following syntax to compile the DFLOWFM model engine source code only: config="dflowfm", generator="Unix Makefiles", compiler="intel21"
2. Go into "src" directory, and edit the "setenv.sh" file to essentially load and link your library paths that are required to compile DFlowFM source code as stated above. *** The current setenv.sh file is already set for the RDHPCS NOAA Hera cluster, so if you're compiling it on there you dont need to change it ***
# Steps below to compile DFlowFM model source code
1. Unpack DFlowFM source code. Go into directory and open the build.sh file. Go to lines 190-192 and change the following syntax to compile the DFLOWFM model engine source code only: config="dflowfm", generator="Unix Makefiles", compiler="intel21"
2. Go into "src" directory, and edit the "setenv.sh" file to essentially load and link your library paths that are required to compile DFlowFM source code as stated above. *** The current setenv.sh file is already set for the RDHPCS NOAA Hera cluster, so if you're compiling it on there you dont need to change it ***
3. Go back into the top directory and execute "./build.sh" to initiate the DFlowFM model to use cmake and build the entire model engine source code from scratch. If successful, you should see that all DFLOWFM model engine modules have been built in the newly created "build_dflowfm" directory. Go into the "build_dflowfm/install" directory and verify that you see executables built within the "bin" directory and shared libraries built within the "lib" directory. If so, then you should be good to go for the next step. If not, consult with the build issues and adjust libraries accordingly.
4. Go into the "build_dflowfm/install" directory and copy over the "lib" directory to the "BMI/dflowfm_install" directory within the top directory of this repository. You will also need to copy over the *.mod files located within the "build_dflowfm/dflowdm_kernel", "build_dflowfm/gridgeom", "build_dflowfm/ec_module", and "build_dflowfm/deltares_common" directories into the "BMI/dflowfm_install/include" directory located within the top directory of this repository.
5. Once the DFlowFM libraries and modules are within the BMI directory, go into that directory and source your entire DFLOWFM environement that you have setup within the "setenv.sh" script. Within the "setenv.sh" script you have set in step #2 above, you should transfer all the commands to load the DFLOWFM libraries into the "dflowfm_source.sh" script so you can source your environment on the fly to quickly compile the BMI libraries. The current "dflowfm_source.sh" script has already been setup for the RDHPCS NOAA Hera cluster, so you don't need to do anything else if you're working on this environment already.
6. You will need to go into the CMakeLists.txt file and update the pathway to where you have placed the "dflowfm_install" directory on your setup. This requires updating lines 66-69 within the CMakeLists.txt file. If you are not using the RDHPCS NOAA Hera cluster, then you will also need to update the metis header and shared libraries on your server as needed within the commands that list the pathway. Otherwise, proceed onto the next step.
7. Make a directory called "build" and go into that directory and execute the command "cmake ..". Once files have been written, execute the command "cmake --build . --target testbmifortranmodel" and this will build the shared BMI library. To build the DFLOWFM BMI driver test, execute the command "cmake --build . --target dflowfm_driver". If no errors occur, then you have built the DFLOWFM BMI libraries!
8. To utilize the DFLOWFM BMI shared libraries and its driver, copy over the "dflowfm_driver" executable built within your "build" directory to a DFLOWFM model setup and the sample "namelist.input" file as well with within the "BMI" directory to your DFLOWFM model setup and adjust accordingly to your model setup specifications to succesfully execute the BMI model.
   
