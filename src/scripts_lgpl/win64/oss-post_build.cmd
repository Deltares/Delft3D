@echo off

rem ######
rem # MAIN
rem ######

setlocal enabledelayedexpansion
set globalErrorLevel=0
set oss_mpi=IntelMPI
rem set oss_mpi=MPICH

echo oss-post_build...

rem Usage:
rem > oss-install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration>
rem > oss-install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> [project]
rem > oss-install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> [project] ["compiler_redist_dir"]
rem > oss-install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> [project] ["compiler_redist_dir"] ["mkl_redist_dir"]
rem > oss-install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> [project] ["compiler_redist_dir"] ["mkl_redist_dir"] ["mpi_redist_dir"]

rem with:
rem   <install_dir>           : Target directory where all binaries etc. are going to be installed by this script
rem   <build_dir>             : The root build directory where all binaries are build
rem   <configuration>         : Debug or release configuration. Configuration determines the directory structure
rem   [project]               : (optional) project to install. If missing, "everything" is installed
rem   ["compiler_redist_dir"] : (optional) Directory containing compiler specific dll's to be installed
rem   ["mkl_redist_dir"]      : (optional) Directory containing Intel math kernel library specific dll's to be installed
rem   ["mpi_redist_dir"]      : (optional) Directory containing Intel MPI library specific dll's to be installed

rem
rem Example calls:
rem > install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration>                                                    # Install entire solution
rem > install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> dflowfm                                            # Install only project dflowfm (and its dependencies)
rem > install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> dflowfm ["compiler_redist_dir"]                    # "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\compiler\"
rem                                                                                                                                # Install only project dflowfm (and its dependencies)
rem > install.cmd <install_dir> <build_dir> <checkout_src_root> <configuration> dflowfm ["compiler_redist_dir"] ["mkl_redist_dir"] # "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\compiler\"  "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\mkl\"
rem                                                                                                                                # Install only project dflowfm (and its dependencies including mkl required dlls)

rem Example calls:
rem >  oss-post_build.cmd install_dir Debug            # postbuild all debug
rem >  oss-post_build.cmd install_dir Debug dimr       # postbuild debug dimr

rem  The next statement is needed in order for the set commands to work inside the if statement

set install_dir=%1
set build_dir=%2
set checkout_src_root=%3
set configuration=%4
set project=%5

rem substitute backslashes
set "install_dir=!install_dir:/=\!"
set "build_dir=!build_dir:/=\!"
set "checkout_src_root=!checkout_src_root:/=\!"

if [%5] EQU [] (
    rem Install all engines
    set project=install_all
)

if [%6] EQU [] (
    set compiler_redist_dir=""
) else (
    set compiler_redist_dir_read=%6
    rem Remove leading and trailing quote (")
    rem These quotes MUST be present in argument number 3, because "compiler_redist_dir" may contain white spaces
    set compiler_redist_dir=!compiler_redist_dir_read:~1,-1!
    set "compiler_redist_dir=!compiler_redist_dir:/=\!"
)

if [%7] EQU [] (
    set mkl_redist_dir=""
) else (
    set mkl_redist_dir_read=%7
    rem Remove leading and trailing quote (")
    rem These quotes MUST be present in argument number 4, because "mkl_redist_dir_read" may contain white spaces
    set mkl_redist_dir=!mkl_redist_dir_read:~1,-1!
    set "mkl_redist_dir=!mkl_redist_dir:/=\!"
)

if [%8] EQU [] (
    set mpi_redist_dir=""
) else (
    set mpi_redist_dir_read=%8
    rem Remove leading and trailing quote (")
    rem These quotes MUST be present in argument number 8, because "mpi_redist_dir_read" may contain white spaces
    set mpi_redist_dir=!mpi_redist_dir_read:~1,-1!
    set "mpi_redist_dir=!mpi_redist_dir:/=\!"
)

IF defined IFORT_COMPILER23 (
    set petsc_dir="!checkout_src_root!\third_party_open\petsc\petsc-3.19.4\lib\x64\Release-oneAPI\*.dll"
) else (
    set petsc_dir="!checkout_src_root!\third_party_open\petsc\petsc-3.10.2\lib\x64\Release-oneAPI\*.dll"
)

echo install_dir         : !install_dir!
echo build_dir           : !build_dir!
echo checkout_src_root   : !checkout_src_root!
echo configuration       : !configuration!
echo project             : !project!
echo compiler_redist_dir : !compiler_redist_dir!
echo mkl_redist_dir      : !mkl_redist_dir!
echo mpi_redist_dir      : !mpi_redist_dir!
echo oss_mpi             : !oss_mpi!
echo petsc_dir           : !petsc_dir!



rem Change to directory tree where this batch file resides (necessary when oss-install.cmd is called from outside of oss/trunk/src)
cd %~dp0\..\..

call :!project!

goto end





rem ############
rem # PROCEDURES
rem ############

rem =============================================================
rem === copyFile takes two arguments: the name of the file to ===
rem === copy to the destiny directory                         ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === with an error code after executing the rest of its    ===
rem === statements                                            ===
rem =============================================================
:copyFile
    set fileName=%~1
    set dest=%~2
    rem
    rem "echo f |" is (only) needed when dest does not exist
    rem and does not harm in other cases
    rem
    echo f | xcopy "%fileName%" %dest% /F /Y
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!fileName!" to "!dest!"
    )
goto :endproc


rem =============================================================
rem === copyFileWithPrefix takes three arguments:             ===
rem === the name of the file to
rem === copy to the destiny directory and its prefix          ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === with an error code after executing the rest of its    ===
rem === statements                                            ===
rem =============================================================
:copyFileWithPrefix
    set fullPath=%~1
    set prefix=%~2
    set dest=%~3

    rem Extract the filename and the path
    for %%F in ("%fullPath%") do (
        set "filePath=%%~dpF"
    )

    rem
    rem "echo f |" is (only) needed when dest does not exist
    rem and does not harm in other cases
    rem Iterate through the matching files and copy them individually
    rem
    for %%G in ("%fullPath%") do (
        set "fileName=%%~nG"
        set "fileExtension=%%~xG"
        set "newName=%prefix%_%%~nG%%~xG"
        echo f | xcopy "%%G" "%dest%\!newName!" /F /Y
    )
goto :endproc


rem ===================================================================
rem === copyDir: Copy the full contents of a directory (source)     ===
rem ===          to another location (target)                       ===
rem ===                                                             ===
rem === Arguments:                                                  ===
rem ===     source: path\name of (existing) directory to be copied  ===
rem ===     target: path\name of (existing) directory to copy to    ===
rem ===                                                             ===
rem === If source and target exist, the full contents of source     ===
rem === will be copied into target.                                 ===
rem ===                                                             ===
rem === NOTE: errors will be reported and the script will           ===
rem === with an error code after executing the rest of its          ===
rem === statements                                                  ===
rem ===================================================================
:copyDir
    set source=%~1
    set target=%~2
    rem
    rem "echo f |" is (only) needed when target does not exist
    rem and does not harm in other cases
    rem
    echo f | xcopy "%source%" %target% /I /E /F /Y
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!source!" to "!target!"
    )
goto :endproc



rem ===============
rem === copyMPI
rem === compare with copyMPIRedist below
rem ===============
:copyMPI
    set destination=%~1
    echo "Copy MPI libraries . . ."
    if "!oss_mpi!" == "IntelMPI" (
        call :copyFile "%I_MPI_ONEAPI_ROOT%\env\*.bat"                !destination!
        call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.dll"                !destination!
        call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.exe"                !destination!
        call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\release\*.dll"        !destination!
        call :copyFile "%I_MPI_ONEAPI_ROOT%\libfabric\bin\*.dll"      !destination!
    )
    if "!oss_mpi!" == "MPICH" (
        call :copyFile "!checkout_src_root!\third_party_open\mpich2\x64\bin\*.exe" !destination!
        call :copyFile "!checkout_src_root!\third_party_open\mpich2\x64\lib\*.dll" !destination!
    )
goto :endproc



rem ===============
rem === copyMPIRedist
rem === compare with copyMPI above
rem ===============
:copyMPIRedist
    if !mpi_redist_dir!=="" (
        rem mpi_redist_dir not set
    ) else (
        echo "Copy MPI redist libraries . . ."
        call :copyFile "!mpi_redist_dir!env\*.bat"                !dest_bin!
        call :copyFile "!mpi_redist_dir!bin\*.dll"                !dest_bin!
        call :copyFile "!mpi_redist_dir!bin\*.exe"                !dest_bin!
        call :copyFile "!mpi_redist_dir!bin\release\*.dll"        !dest_bin!
        call :copyFile "!mpi_redist_dir!libfabric\bin\*.dll"      !dest_bin!
    )
goto :endproc



rem ===============
rem === copyCompilerRedist
rem === if argument = "withPetsc":
rem ===       also copy fitting Petsc library
rem ===============
:copyCompilerRedist
    set argument=%~1
    if !compiler_redist_dir!=="" (
        rem Compiler_dir not set
    ) else (
        echo "Copy Compiler redist libraries . . ."
        rem "Compiler_dir:!compiler_redist_dir!"
        set localstring="!compiler_redist_dir!*.dll"
        rem Note the awkward usage of !-characters
        call :copyFile !!localstring! !dest_bin!!
        if "!argument!" == "withPetsc" (
            call :copyFile "!petsc_dir!"               !dest_bin!
            call :copyFile "!petsc_dir!"               !dest_share!
        )
    )
goto :endproc



rem ===============
rem === copyMklRedist
rem === if argument = "withPetsc":
rem ===       also copy fitting Petsc library
rem ===============
:copyMklRedist
    set argument=%~1
    if !mkl_redist_dir!=="" (
        rem mkl_redist_dir not set
    ) else (
        echo "Copy MKL redist libraries . . ."
        rem note that for oneAPI MKL, the DLL names end in '.1.dll'
        set localstring="!mkl_redist_dir!mkl_core*.dll"
        call :copyFile !!localstring! !dest_bin!
        set localstring="!mkl_redist_dir!mkl_def*.dll"
        call :copyFile !!localstring! !dest_bin!
        set localstring="!mkl_redist_dir!mkl_core*.dll"
        call :copyFile !!localstring! !dest_bin!
        set localstring="!mkl_redist_dir!mkl_avx*.dll"
        call :copyFile !!localstring! !dest_bin!
        rem is needed for dimr nuget package? please check
        call :copyFile !!localstring! !dest_share!
        set localstring="!mkl_redist_dir!mkl_intel_thread*.dll"
        call :copyFile !!localstring! !dest_bin!
        call :copyFile !!localstring! !dest_share!
        set localstring="!mkl_redist_dir!mkl_sequential*.dll"
        call :copyFile !!localstring! !dest_bin!
        if "!argument!" == "withPetsc" (
            rem if 'mkl_redist_dir' contains 'oneAPI', use the version of petsc built with oneAPI Fortran

            call :copyFile "!petsc_dir!"             !dest_bin!
            )
        )
    )
goto :endproc



rem ===============
rem === POSTBUILD_ALL
rem ===============
:install_all
    echo " postbuild all open source projects . . ."

    call :dimr
    call :dimr_dll
    call :dflowfm
    call :dflowfm_dll
    call :dfmoutput
    call :dfm_volume_tool
    call :cosumo_bmi
    call :mormerge
    call :waq_plugin_wasteload
    call :delwaq_lib
    call :delwaq1
    call :delwaq2
    call :waqpb_export
    call :waqpb_import
    call :waqmerge
    call :ddcouple
    call :agrhyd
    call :maptonetcdf
    call :duprol2delwaq
    call :delpar
    call :wave
    call :wave_exe
    call :cf_dll
    call :flow1d2d
    call :d_hydro
    call :flow2d3d
    call :plugin_culvert
    call :plugin_delftflow_traform
    call :rr_dll
    call :rtc
    call :nesthd1
    call :nesthd2
    call :datsel
    call :kubint
    call :lint

goto :endproc



rem =============================================================
rem === makeDir accepts one argument: the name of the         ===
rem === directory it will create if it doesn't already exists ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === return with an error code after executing the rest of ===
rem === its statements                                        ===
rem =============================================================
:makeDir
    set dirName=%~1
    if not exist !dirName! mkdir !dirName!
    if not !ErrorLevel! EQU 0 (
        echo ERROR: while creating directory "!dirName!"
    )
goto :endproc



rem =============================================================
rem === makeAllDirs makes all needed directories              ===
rem =============================================================
:makeAllDirs

    call :makeDir !dest_bin!
    call :makeDir !dest_default!
    call :makeDir !dest_scripts!
    call :makeDir !dest_plugins!
    call :makeDir !dest_share!

goto :endproc



rem =============================================================
rem === setWaqFolders sets all folders               for Waq  ===
rem =============================================================
:setWaqFolders

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dwaq\bin"
        set dest_default="!install_dir!\x64\Release\dwaq\resources"
        set dest_scripts="!install_dir!\x64\Release\dwaq\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"
     )

goto :endproc



rem =============================================================
rem === copyWaqProcessFiles copies all process files for Waq  ===
rem =============================================================
:copyWaqProcessFiles

    set destination=%~1
    set destination_csv=%~2
    set destination_sub="!destination!\subFiles"

    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\process_lib\bloom.spe"                                   !destination!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\process_lib\proc_def.dat"                                !destination!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\process_lib\proc_def.def"                                !destination!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\process_lib\csvFiles\*"                                  !destination_csv!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\process_lib\subFiles\*"                                  !destination_sub!

goto :endproc



rem =============================================================
rem === copies runtime libraries for dflowfm and dflowfm_dll  ===
rem =============================================================
:copyDflowfmDependentRuntimeLibraries

    echo "copyDflowfmDependentRuntimeLibraries . . ."

    call :copyMPI                                                                                                            !dest_share!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                          !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                             !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\expat\x64\x64\%configuration%\libexpat.dll"                         !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\\*.dll"                                            !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\Tecplot\lib\x64\\*.dll"                                             !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\xerces-c_3_2.dll"                 !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\gdal300.dll"                      !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\expat.dll"                        !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\libpq.dll"                        !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\sqlite3.dll"                      !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\libmysql.dll"                     !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\spatialite.dll"                   !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\proj.dll"                         !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\proj_6_1.dll"                     !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\openjp2.dll"                      !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\geos_c.dll"                       !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\libxml2.dll"                      !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\iconv.dll"                        !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\geos.dll"                         !dest_bin!
    call :copyFile "!checkout_src_root!\third_party_open\GISInternals\release-1911-x64\bin\freexl.dll"                       !dest_bin!

    call :copyCompilerRedist withPetsc
    call :copyMklRedist withPetsc
    call :copyMPIRedist


goto :endproc



rem =============================================================
rem === copies runtime libraries for dimr and dimr_lib        ===
rem =============================================================
:copyDimrDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!
    call :copyFile "!checkout_src_root!\third_party_open\expat\x64\x64\%configuration%\libexpat.dll"                    !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for DWaq                     ===
rem =============================================================
:copyDwaqDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!

    rem copy intel dlls
    call :copyCompilerRedist noPetsc


goto :endproc




rem =============================================================
rem === copies runtime libraries for DWaves                   ===
rem =============================================================
:copyDwavesDependentRuntimeLibraries

    set destination=%~1
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for SwanOMP                  ===
rem =============================================================
:copySwanOmpDependentRuntimeLibraries

    set destination=%~1
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for SwanMPI                  ===
rem =============================================================
:copySwanMpiDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for flow1d                   ===
rem =============================================================
:copyFlow1DDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for flow1d2d                 ===
rem =============================================================
:copyFlow1D2DDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for D_HYDRO        ===
rem =============================================================
:copyDHydroDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!
    call :copyFile "!checkout_src_root!\third_party_open\expat\x64\x64\%configuration%\libexpat.dll"                    !destination!

    call :copyCompilerRedist noPetsc
    call :copyMklRedist noPetsc
    call :copyMPIRedist

goto :endproc



rem =============================================================
rem === copies runtime libraries for flow2d3d                 ===
rem =============================================================
:copyFlow2D3DDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!
    call :copyFile "!checkout_src_root!\third_party_open\expat\x64\x64\%configuration%\libexpat.dll"                    !destination!

    call :copyCompilerRedist noPetsc
    call :copyMklRedist noPetsc
    call :copyMPIRedist
goto :endproc



rem =============================================================
rem === copies runtime libraries for nesthd1                  ===
rem =============================================================
:copyNestHD1DependentRuntimeLibraries

    set destination=%~1

goto :endproc



rem =============================================================
rem === copies runtime libraries for nesthd2                  ===
rem =============================================================
:copyNestHD2DependentRuntimeLibraries

    set destination=%~1

goto :endproc



rem =============================================================
rem === copies runtime libraries for datsel                   ===
rem =============================================================
:copyDatselDependentRuntimeLibraries

    set destination=%~1

goto :endproc



rem =============================================================
rem === copies runtime libraries for kubint                   ===
rem =============================================================
:copyKubintDependentRuntimeLibraries

    set destination=%~1

goto :endproc



rem =============================================================
rem === copies runtime libraries for lint                     ===
rem =============================================================
:copyLintDependentRuntimeLibraries

    set destination=%~1

goto :endproc



rem =============================================================
rem === copies runtime libraries for RR                       ===
rem =============================================================
:copyRRDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\netcdf\netCDF 4.6.1\bin\*"                                     !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!
    call :copyFile "!checkout_src_root!\third_party_open\expat\x64\x64\%configuration%\libexpat.dll"                    !destination!

goto :endproc



rem =============================================================
rem === copies runtime libraries for RTC                      ===
rem =============================================================
:copyRTCDependentRuntimeLibraries

    set destination=%~1
    call :copyMPI                                                                                                       !destination!
    call :copyFile "!checkout_src_root!\third_party_open\pthreads\bin\x64\*.dll"                                        !destination!

goto :endproc



rem ==========================
rem === POST_BUILD_DFLOWFM_DLL
rem ==========================
:dflowfm_dll

    echo "postbuild dflowfm_dll . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDflowfmDependentRuntimeLibraries

        rem copy binaries and dll
        call :copyFile "!build_dir!\dflowfm_lib\!configuration!\dflowfm.*"                                           !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_default="!install_dir!\x64\Release\dflowfm\default"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyDflowfmDependentRuntimeLibraries

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyDflowfmDependentRuntimeLibraries
        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\dflowfm_lib\!configuration!\dflowfm.dll"                                           !dest_bin!

    )

goto :endproc



rem ==========================
rem === POST_BUILD_DFLOWFM-CLI
rem ==========================
:dflowfm-cli
    echo "postbuild dflowfm-cli . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDflowfmDependentRuntimeLibraries
        call :copyFile "!build_dir!\dflowfm_cli_exe\!configuration!\dflowfm-cli.*"                                            !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_default="!install_dir!\x64\Release\dflowfm\default"
        set dest_csvFiles="!install_dir!\x64\Release\dflowfm\default\csvFiles"
        set dest_subFiles="!install_dir!\x64\Release\dflowfm\default\subFiles"
        set dest_default_backup="!install_dir!\x64\Release\dflowfm\installation_default"
        set dest_csvFiles_backup="!install_dir!\x64\Release\dflowfm\installation_default\csvFiles"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :makeDir !dest_csvFiles!
        call :makeDir !dest_subFiles!
        call :makeDir !dest_default_backup!
        call :makeDir !dest_csvFiles_backup!
        call :copyDflowfmDependentRuntimeLibraries

        call :copyFile "!build_dir!\dflowfm_cli_exe\!configuration!\dflowfm-cli.*"                                           !dest_bin!

        call :copyWaqProcessFiles !dest_default! !dest_csvFiles!

        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\team-city\run_dflowfm_processes.bat"               !dest_scripts!
        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\team-city\run_dflowfm.bat"                         !dest_scripts!
    )

goto :endproc



rem =================================
rem === POST_BUILD_DFLOWFM_INTERACTER
rem =================================
:dflowfm_interacter
    echo "postbuild dflowfm_interacter . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDflowfmDependentRuntimeLibraries
        call :copyFile "!build_dir!\dflowfm\!configuration!\dflowfm.*"                                                   !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_default="!install_dir!\x64\Release\dflowfm\default"
        set dest_csvFiles="!install_dir!\x64\Release\dflowfm\default\csvFiles"
        set dest_subFiles="!install_dir!\x64\Release\dflowfm\default\subFiles"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :makeDir !dest_csvFiles!
        call :makeDir !dest_subFiles!
        call :makeDir !dest_default_backup!
        call :makeDir !dest_csvFiles_backup!
        call :copyDflowfmDependentRuntimeLibraries

        call :copyFile "!build_dir!\dflowfm\!configuration!\dflowfm.exe"                                                   !dest_bin!

        call :copyWaqProcessFiles !dest_default! !dest_csvFiles!


        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\MSDOS\run_dflowfm_processes.bat"                   !dest_scripts!
        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\team-city\run_dflowfm.bat"                         !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_DFMOUTPUT
rem ==========================
:dfmoutput
    echo "postbuild dfmoutput . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin=    "!install_dir!\x64\Debug\"
        set dest_default="!install_dir!\x64\Debug\"
        set dest_scripts="!install_dir!\x64\Debug\"
        set dest_plugins="!install_dir!\x64\Debug\"
        set dest_share=  "!install_dir!\x64\Debug\"

        call :makeDir !dest_bin!
        call :copyDflowfmDependentRuntimeLibraries
        call :copyFile "!build_dir!\dfmoutput\!configuration!\dfmoutput.*"                                                    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_default="!install_dir!\x64\Release\dflowfm\default"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyDflowfmDependentRuntimeLibraries

        call :copyFile "!build_dir!\dfmoutput\!configuration!\dfmoutput.exe"                                                   !dest_bin!

        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\team-city\run_dfmoutput.bat"                       !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_dfm_volume_tool
rem ==========================
:dfm_volume_tool
    echo "postbuild dfm_volume_tool . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDflowfmDependentRuntimeLibraries
        call :copyFile "!build_dir!\dfm_volume_tool\!configuration!\dfm_volume_tool.*"                                                    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_default="!install_dir!\x64\Release\dflowfm\default"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyDflowfmDependentRuntimeLibraries

        call :copyFile "!build_dir!\dfm_volume_tool\!configuration!\dfm_volume_tool.exe"                                                   !dest_bin!

        call :copyFile "!checkout_src_root!\engines_gpl\dflowfm\scripts\team-city\run_dfm_volume_tool.bat"                       !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_MORMERGE
rem ==========================
:mormerge
    echo "postbuild mormerge . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        rem call :copyFile "!build_dir!\mormerge\!configuration!\mormerge.*"                               !dest_bin!
        call :copyFile "!checkout_src_root!\tools_gpl\mormerge\scripts\mormerge.tcl"                       !dest_bin!
        call :copyFile "!checkout_src_root!\third_party_open\tcl\bin\win64\tclkitsh852.exe"                !dest_share!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dmor\bin"
        set dest_scripts="!install_dir!\x64\Release\dmor\scripts"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_scripts!
        call :copyFile "!build_dir!\mormerge\!configuration!\mormerge.*"                                   !dest_bin!
        call :copyFile "!checkout_src_root!\tools_gpl\mormerge\scripts\mormerge.tcl"                       !dest_scripts!
        call :copyFile "!checkout_src_root!\tools_gpl\mormerge\scripts\run_mormerge.bat"                   !dest_scripts!
        call :copyFile "!checkout_src_root!\third_party_open\tcl\bin\win64\tclkitsh852.exe"                !dest_share!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_COSUMO_BMI
rem ==========================
:cosumo_bmi
    echo "postbuild cosumo_bmi . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        call :makeDir !dest_bin!
        call :copyFile "!build_dir!\cosumo_bmi\!configuration!\cosumo_bmi.*"                               !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflowfm\bin"
        set dest_scripts="!install_dir!\x64\Release\dflowfm\scripts"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_scripts!
        call :copyFile "!build_dir!\cosumo_bmi\!configuration!\cosumo_bmi.*"                               !dest_bin!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_DIMR
rem ==========================
:dimr
    echo "postbuild dimr . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyDimrDependentRuntimeLibraries                                                                               !dest_bin!
    call :copyFile "!build_dir!\dimr\!configuration!\dimr.*"                                                              !dest_bin!
    )


    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\dimr\bin"
    set dest_default="!install_dir!\x64\Release\dimr\default"
    set dest_scripts="!install_dir!\x64\Release\dimr\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"
    set dest_schema="!install_dir!\x64\Release\dimr\schema"

    call :makeAllDirs
    call :copyDimrDependentRuntimeLibraries                                                                             !dest_share!
    call :copyFile "!build_dir!\dimr\!configuration!\dimr.exe"                                                          !dest_bin!

    call :copyFile "!checkout_src_root!\engines_gpl\dimr\scripts\generic\win64\*.*"                                     !dest_scripts!
    call :copyDir  "!checkout_src_root!\engines_gpl\dimr\schemas"                                                       !dest_schema!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_DIMR_LIB
rem ==========================
:dimr_lib
    echo "postbuild dimr_lib . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyDimrDependentRuntimeLibraries                                                                               !dest_bin!
    call :copyFile "!build_dir!\dimr_lib\!configuration!\dimr_dll.*"                                                      !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\dimr\bin"
    set dest_default="!install_dir!\x64\Release\dimr\default"
    set dest_scripts="!install_dir!\x64\Release\dimr\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeAllDirs
    call :copyDimrDependentRuntimeLibraries                                                                               !dest_share!
    call :copyFile "!build_dir!\dimr_lib\!configuration!\dimr_dll.*"                                                      !dest_bin!

    )

goto :endproc



rem ===================================
rem === POST_BUILD_waq_plugin_wasteload
rem ===================================
:waq_plugin_wasteload

    echo "postbuild waq_plugin_wasteload . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                                             !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\waq_plugin_wasteload\!configuration!\waq_plugin_wasteload.*"                               !dest_bin!

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\waq_plugin_wasteload\PGOPTI.*" waq_plugin_wasteload      !dest_bin!
goto :endproc



rem ==========================
rem === POST_BUILD_delwaq_lib
rem ==========================
:delwaq_lib

    echo "postbuild delwaq_lib . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                        !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\delwaq_lib\!configuration!\delwaq.*"                               !dest_bin!
    call :copyFile "!build_dir!\waq_plugin_wasteload\!configuration!\waq_plugin_wasteload.*"       !dest_bin!

    if "%configuration%" == "Release" (

        set dest_csvFiles="!install_dir!\x64\Release\dwaq\resources\csvFiles"
        set dest_subFiles="!install_dir!\x64\Release\dwaq\resources\subFiles"
        set dest_default_backup="!install_dir!\x64\Release\dwaq\installation_default"
        set dest_csvFiles_backup="!install_dir!\x64\Release\dwaq\installation_default\csvFiles"

        call :makeDir !dest_csvFiles!
        call :makeDir !dest_subFiles!
        call :makeDir !dest_default_backup!
        call :makeDir !dest_csvFiles_backup!

        rem copy waq process files
        call :copyWaqProcessFiles !dest_default! !dest_csvFiles!
        call :copyWaqProcessFiles !dest_default_backup! !dest_csvFiles_backup!

        rem profiling information
        call :copyFileWithPrefix "!build_dir!\delwaq_lib\PGOPTI.*" delwaq_lib      !dest_bin!
    )
goto :endproc



rem ==========================
rem === POST_BUILD_delwaq1
rem ==========================
:delwaq1

    echo "postbuild delwaq1 . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                       !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\delwaq_lib\!configuration!\delwaq.*"                              !dest_bin!
    call :copyFile "!build_dir!\delwaq1\!configuration!\delwaq1.*"                                !dest_bin!
    call :copyFile "!build_dir!\waq_plugin_wasteload\!configuration!\waq_plugin_wasteload.*"      !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\scripts\run_delwaq.bat"     !dest_scripts!
    )

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\delwaq1\PGOPTI.*" delwaq1      !dest_bin!

goto :endproc



rem ==========================
rem === POST_BUILD_delwaq2
rem ==========================
:delwaq2

    echo "postbuild delwaq2 . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                         !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\delwaq_lib\!configuration!\delwaq.*"                                !dest_bin!
    call :copyFile "!build_dir!\delwaq2\!configuration!\delwaq2.*"                                  !dest_bin!
    call :copyFile "!build_dir!\waq_plugin_wasteload\!configuration!\waq_plugin_wasteload.*"        !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\scripts\run_delwaq.bat"       !dest_scripts!
    )

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\delwaq2\PGOPTI.*" delwaq2      !dest_bin!
goto :endproc



rem ==========================
rem === POST_BUILD_waqpb_export
rem ==========================
:waqpb_export

    echo "postbuild waqpb_export . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                         !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\waqpb_export\!configuration!\waqpb_export.*"                        !dest_bin!

    rem copy run scripts
    call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\waqpb\resources\scripts\run_waqpb_export.bat"               !dest_scripts!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\scripts\export_procdef_csvfiles.bat"        !dest_scripts!

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\waqpb_export\PGOPTI.*" waqpb_export      !dest_bin!
goto :endproc



rem ==========================
rem === POST_BUILD_waqpb_import
rem ==========================
:waqpb_import

    echo "postbuild waqpb_import . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                               !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\waqpb_import\!configuration!\waqpb_import.*"              !dest_bin!

    rem copy run scripts
    call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\waqpb\resources\scripts\run_waqpb_import.bat"        !dest_scripts!
    call :copyFile "!checkout_src_root!\engines_gpl\waq\resources\scripts\import_procesasc_changes.bat"            !dest_scripts!

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\waqpb_import\PGOPTI.*" waqpb_import      !dest_bin!
goto :endproc




rem ==========================
rem === POST_BUILD_duprol2delwaq
rem ==========================
:duprol2delwaq

    echo "postbuild duprol2delwaq . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                 !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\duprol2delwaq\!configuration!\duprol2delwaq.*"              !dest_bin!

goto :endproc



rem ==========================
rem === POST_BUILD_delpar
rem ==========================
:delpar

    echo "postbuild delpar . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!

        rem copy binaries and dll
        call :copyDwaqDependentRuntimeLibraries                                                 !dest_share!
        call :copyFile "!build_dir!\delpar\!configuration!\delpar.*"                            !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dpart\bin"
        set dest_default="!install_dir!\x64\Release\dpart\default"
        set dest_scripts="!install_dir!\x64\Release\dpart\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs

        rem copy binaries and dll
        call :copyDwaqDependentRuntimeLibraries                                                 !dest_share!
        call :copyFile "!build_dir!\delpar\!configuration!\delpar.exe"                          !dest_bin!

        call :copyFile "!checkout_src_root!\engines_gpl\part\resources\scripts\run_dpart.bat"    !dest_scripts!
    )

    rem profiling information
    call :copyFileWithPrefix "!build_dir!\delpar\PGOPTI.*" delpar      !dest_bin!

goto :endproc



rem ==========================
rem === POST_BUILD_waqmerge
rem ==========================
:waqmerge

    echo "postbuild waqmerge . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                     !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\waqmerge\!configuration!\waqmerge.exe"                          !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\waqmerge\resources\scripts\run_waqmerge.bat"        !dest_scripts!
    )
goto :endproc



rem ==========================
rem === POST_BUILD_ddcouple
rem ==========================
:ddcouple

    echo "postbuild ddcouple . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                       !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\ddcouple\!configuration!\ddcouple.exe"                            !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\ddcouple\resources\scripts\run_ddcouple.bat"          !dest_scripts!
    )
goto :endproc



rem ==========================
rem === POST_BUILD_agrhyd
rem ==========================
:agrhyd

    echo "postbuild agrhyd . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                       !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\agrhyd\!configuration!\agrhyd.exe"                                !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\agrhyd\resources\default\agrhyd.ini"                  !dest_default!
        call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\agrhyd\resources\scripts\run_agrhyd.bat"              !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_maptonetcdf
rem ==========================
:maptonetcdf

    echo "postbuild maptonetcdf . . ."

    call :setWaqFolders

    call :makeAllDirs
    call :copyDwaqDependentRuntimeLibraries                                                       !dest_share!

    rem copy binaries and dll
    call :copyFile "!build_dir!\maptonetcdf\!configuration!\maptonetcdf.exe"                      !dest_bin!

    if "%configuration%" == "Release" (
        call :copyFile "!checkout_src_root!\tools_gpl\waq_tools\maptonetcdf\resources\scripts\run_maptonetcdf.bat"    !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_wave
rem ==========================
:wave

    echo "postbuild wave . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDwavesDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\wave\!configuration!\wave.*"                                                          !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dwaves\bin"
        set dest_default="!install_dir!\x64\Release\dwaves\default"
        set dest_scripts="!install_dir!\x64\Release\dwaves\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"
        set dest_bin_esmf="!install_dir!\x64\Release\esmf\bin"
        set dest_scripts_esmf="!install_dir!\x64\Release\esmf\scripts"

        call :makeAllDirs
        call :makeDir !dest_bin_esmf!
        call :makeDir !dest_scripts_esmf!
        call :copyDwavesDependentRuntimeLibraries                                                                       !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\wave\!configuration!\wave.*"                                                        !dest_bin!
        call :copyFile "!checkout_src_root!\third_party_open\esmf\win64\bin\*"                                          !dest_bin_esmf!
        call :copyFile "!checkout_src_root!\third_party_open\esmf\win64\scripts\*"                                      !dest_scripts_esmf!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_wave_exe
rem ==========================
:wave_exe

    echo "postbuild wave_exe. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDwavesDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\wave\!configuration!\wave_exe.*"                                                      !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dwaves\bin"
        set dest_default="!install_dir!\x64\Release\dwaves\default"
        set dest_scripts="!install_dir!\x64\Release\dwaves\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyDwavesDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\wave\!configuration!\wave_exe.exe"                                                      "!install_dir!\x64\Release\dwaves\bin\wave.exe"

        call :copyFile "!checkout_src_root!\engines_gpl\wave\scripts\run_dwaves.bat"                                        !dest_scripts!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_swan_omp
rem ==========================
:swan_omp

    echo "postbuild swan_omp . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copySwanOmpDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\swan_omp\!configuration!\swan_omp.*"                                                     !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\swan\bin"
        set dest_default="!install_dir!\x64\Release\swan\default"
        set dest_scripts="!install_dir!\x64\Release\swan\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copySwanOmpDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\swan_omp\!configuration!\swan_omp.*"                                                     !dest_bin!

        rem copy script
        call :copyFile "!checkout_src_root!\third_party_open\swan\scripts\swan.bat"                                          !dest_scripts!

    )

goto :endproc



rem ==========================
rem === POST_BUILD_swan_mpi
rem ==========================
:swan_mpi

    echo "postbuild swan_mpi . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copySwanMpiDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\swan_mpi\!configuration!\swan_mpi.*"                                                     !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\swan\bin"
        set dest_default="!install_dir!\x64\Release\swan\default"
        set dest_scripts="!install_dir!\x64\Release\swan\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copySwanMpiDependentRuntimeLibraries                                                                           !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\swan_mpi\!configuration!\swan_mpi.*"                                                     !dest_bin!

        rem copy script
        call :copyFile "!checkout_src_root!\third_party_open\swan\scripts\swan.bat"                                          !dest_scripts!
    )

goto :endproc



rem ==========================
rem === cf_dll (flow1d)
rem ==========================
:cf_dll

    echo "postbuild cf_dll . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyFlow1DDependentRuntimeLibraries                                                                             !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\delftflow\!configuration!\cf_dll.*"                                                      !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\flow1d\bin"
        set dest_default="!install_dir!\x64\Release\flow1d\default"
        set dest_scripts="!install_dir!\x64\Release\flow1d\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyFlow1DDependentRuntimeLibraries                                                                             !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyFlow1DDependentRuntimeLibraries                                                                             !dest_bin!
        set dest_bin="!install_dir!\x64\Release\flow1d\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\delftflow\!configuration!\cf_dll.*"                                                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === flow1d2d
rem ==========================
:flow1d2d

    echo "postbuild flow1d2d . . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyRFlow1D2DDependentRuntimeLibraries                                                                             !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\flow1d2d\!configuration!\flow1d2d.*"                                                      !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\flow1d2d\bin"
        set dest_default="!install_dir!\x64\Release\flow1d2d\default"
        set dest_scripts="!install_dir!\x64\Release\flow1d2d\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyRFlow1D2DDependentRuntimeLibraries                                                                             !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyRFlow1D2DDependentRuntimeLibraries                                                                             !dest_bin!
        set dest_bin="!install_dir!\x64\Release\drr\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\flow1d2d\!configuration!\flow1d2d.*"                                                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_D_HYDRO
rem ==========================
:d_hydro
    echo "postbuild d_hydro . . ."
    if "%configuration%" == "Debug" (
        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        call :makeDir !dest_bin!
        call :copyDHydroDependentRuntimeLibraries                                                             !dest_bin!
        call :copyFile "!build_dir!\d_hydro\!configuration!\d_hydro.*"                                        !dest_bin!
    )
    if "%configuration%" == "Release" (
        echo "Release postbuild"
        set dest_bin="!install_dir!\x64\Release\d_hydro\bin"
        set dest_default="!install_dir!\x64\Release\d_hydro\default"
        set dest_scripts="!install_dir!\x64\Release\d_hydro\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"
        set dest_schema="!install_dir!\x64\Release\d_hydro\schema"
        call :makeAllDirs
        call :copyDHydroDependentRuntimeLibraries                                                             !dest_share!
        call :copyFile "!build_dir!\d_hydro\!configuration!\d_hydro.exe"                                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_flow2d3d
rem ==========================
:flow2d3d
:flow2d3d_sp
    echo "postbuild flow2d3d. . ."
    if "%configuration%" == "Debug" (
        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"
        call :makeDir !dest_bin!
        call :copyFlow2D3DDependentRuntimeLibraries                                                       !dest_bin!
        rem copy binaries and dll
        rem take care of both flow2d3d and flow2d3d_sp
        call :copyFile "!build_dir!\flow2d3d\!configuration!\flow2d3d*.*"                                  !dest_bin!
    )
    if "%configuration%" == "Release" (
        echo "Release postbuild"
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_default="!install_dir!\x64\Release\dflow2d3d\default"
        set dest_scripts="!install_dir!\x64\Release\dflow2d3d\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"
        call :makeAllDirs
        call :copyFlow2D3DDependentRuntimeLibraries                                                       !dest_bin!
        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyFlow2D3DDependentRuntimeLibraries                                                       !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        rem copy binaries and dll
        rem take care of both flow2d3d and flow2d3d_sp
        call :copyFile "!build_dir!\flow2d3d\!configuration!\flow2d3d*.*"                                  !dest_bin!
        call :copyFile "!checkout_src_root!\engines_gpl\flow2d3d\scripts\*.bat"                           !dest_scripts!
        call :copyFile "!checkout_src_root!\engines_gpl\flow2d3d\scripts\*.m"                             !dest_scripts!
        call :copyFile "!checkout_src_root!\engines_gpl\flow2d3d\default\*"                               !dest_default!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_plugin_culvert
rem ==========================
:plugin_culvert

    echo "postbuild plugin_culvert. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\plugin_culvert\!configuration!\plugin_culvert.*"                                  !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_default="!install_dir!\x64\Release\dflow2d3d\default"
        set dest_scripts="!install_dir!\x64\Release\dflow2d3d\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs

        rem copy binaries and dll
        call :copyFile "!build_dir!\plugin_culvert\!configuration!\plugin_culvert.dll"                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === POST_BUILD_plugin_delftflow_traform
rem ==========================
:plugin_delftflow_traform

    echo "postbuild plugin_delftflow_traform. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

        set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\plugin_delftflow_traform\!configuration!\plugin_delftflow_traform.*"                                  !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_default="!install_dir!\x64\Release\dflow2d3d\default"
        set dest_scripts="!install_dir!\x64\Release\dflow2d3d\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs

        rem copy binaries and dll
        call :copyFile "!build_dir!\plugin_delftflow_traform\!configuration!\plugin_delftflow_traform.dll"                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === nesthd1
rem ==========================
:nesthd1

    echo "postbuild nesthd1. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!
        call :copyNestHD1DependentRuntimeLibraries    !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\nesthd1\!configuration!\nesthd1.*"    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!

        call :copyNestHD1DependentRuntimeLibraries    !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyNestHD1DependentRuntimeLibraries    !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"

        call :copyFile "!build_dir!\nesthd1\!configuration!\nesthd1.*"    !dest_bin!
    )

goto :endproc



rem ==========================
rem === nesthd2
rem ==========================
:nesthd2

    echo "postbuild nesthd2. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyNestHD2DependentRuntimeLibraries    !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\nesthd2\!configuration!\nesthd2.*"    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!
        call :copyNestHD2DependentRuntimeLibraries    !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyNestHD2DependentRuntimeLibraries    !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\nesthd2\!configuration!\nesthd2.*"    !dest_bin!
    )

goto :endproc



rem ==========================
rem === datsel
rem ==========================
:datsel

    echo "postbuild datsel. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyDatselDependentRuntimeLibraries    !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\datsel\!configuration!\datsel.*"    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!
        call :copyDatselDependentRuntimeLibraries    !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyDatselDependentRuntimeLibraries    !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\datsel\!configuration!\datsel.*"    !dest_bin!
    )

goto :endproc



rem ==========================
rem === kubint
rem ==========================
:kubint

    echo "postbuild kubint. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyKubintDependentRuntimeLibraries    !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\kubint\!configuration!\kubint.*"    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!
        call :copyKubintDependentRuntimeLibraries    !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyKubintDependentRuntimeLibraries    !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\kubint\!configuration!\kubint.*"    !dest_bin!
    )

goto :endproc



rem ==========================
rem === lint
rem ==========================
:lint

    echo "postbuild lint. . ."

    if "%configuration%" == "Debug" (

        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

        call :makeDir !dest_bin!
        call :copyLintDependentRuntimeLibraries    !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\lint\!configuration!\lint.*"    !dest_bin!
    )

    if "%configuration%" == "Release" (

        echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeDir !dest_bin!
        call :makeDir !dest_share!
        call :copyLintDependentRuntimeLibraries    !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyLintDependentRuntimeLibraries    !dest_bin!
        set dest_bin="!install_dir!\x64\Release\dflow2d3d\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\lint\!configuration!\lint.*"    !dest_bin!
    )

goto :endproc



rem ==========================
rem === rr_dll
rem ==========================
:rr_dll

    echo "postbuild rr_dll. . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

    set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!
        call :copyRRDependentRuntimeLibraries                                                                             !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\rr_dll\!configuration!\rr_dll.*"                                                      !dest_bin!
    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\drr\bin"
        set dest_default="!install_dir!\x64\Release\drr\default"
        set dest_scripts="!install_dir!\x64\Release\drr\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyRRDependentRuntimeLibraries                                                                             !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyRRDependentRuntimeLibraries                                                                             !dest_bin!
        set dest_bin="!install_dir!\x64\Release\drr\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\rr_dll\!configuration!\rr_dll.*"                                                      !dest_bin!
    )

goto :endproc



rem ==========================
rem === rtc
rem ==========================
:rtc

    echo "Postbuild RTC (Real Time Control . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
        set dest_bin="%install_dir%\x64\Debug"

    set dest_bin="!install_dir!\x64\Debug"
        set dest_default="!install_dir!\x64\Debug"
        set dest_scripts="!install_dir!\x64\Debug"
        set dest_plugins="!install_dir!\x64\Debug"
        set dest_share="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!
        call :copyRTCDependentRuntimeLibraries                                                                             !dest_bin!

        rem copy binaries and dll
        call :copyFile "!build_dir!\rtc\!configuration!\rtc.*"                                                      !dest_bin!
    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

        set dest_bin="!install_dir!\x64\Release\rtc\bin"
        set dest_default="!install_dir!\x64\Release\rtc\default"
        set dest_scripts="!install_dir!\x64\Release\rtc\scripts"
        set dest_plugins="!install_dir!\x64\Release\plugins\bin"
        set dest_share="!install_dir!\x64\Release\share\bin"

        call :makeAllDirs
        call :copyRTCDependentRuntimeLibraries                                                                             !dest_bin!

        rem Temporarily rename dest_bin to share_bin to copy libraries there as well
        set dest_bin=!dest_share!
        call :copyRTCDependentRuntimeLibraries                                                                             !dest_bin!
        set dest_bin="!install_dir!\x64\Release\rtc\bin"

        rem copy binaries and dll
        call :copyFile "!build_dir!\rtc\!configuration!\rtc.*"                                                      !dest_bin!
    )

goto :endproc



rem ===========================
rem === POST_BUILD_GRIDGEOM_DLL
rem ===========================
:gridgeom_dll
    echo "postbuild gridgeom_dll. . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    xcopy "!build_dir!\gridgeom_dll\!configuration!\gridgeom_dll.dll" "!dest_bin!\gridgeom.dll"* /Y

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeDir !dest_share!
    xcopy "!build_dir!\gridgeom_dll\!configuration!\gridgeom_dll.dll" "!dest_share!\gridgeom.dll"* /Y
    )

goto :endproc



rem ==================================
rem === POST_BUILD_DFLOWFM_KERNEL_TEST
rem ==================================
:dflowfm_kernel_test
    echo "postbuild dflowfm_kernel_test . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\test_dflowfm_kernel\!configuration!\dflowfm_kernel_test.*"                              !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\tests\bin"
    set dest_default="!install_dir!\x64\Release\tests\default"
    set dest_scripts="!install_dir!\x64\Release\tests\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeAllDirs
    call :copyFile "!build_dir!\test_dflowfm_kernel\!configuration!\dflowfm_kernel_test.*"                                !dest_bin!

    )

goto :endproc



rem ===============================
rem === POST_BUILD_WAQ_UTILS_F_TEST
rem ===============================
:waq_utils_f_test
    echo "postbuild waq_utils_f_test . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\test_waq_utils_f\!configuration!\waq_utils_f_test.*"                              !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\tests\bin"
    set dest_default="!install_dir!\x64\Release\tests\default"
    set dest_scripts="!install_dir!\x64\Release\tests\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeAllDirs
    call :copyFile "!build_dir!\test_waq_utils_f\!configuration!\waq_utils_f_test.*"                                !dest_bin!

    )

goto :endproc



rem =============================
rem === POST_BUILD_EC_MODULE_TEST
rem =============================
:ec_module_test
    echo "postbuild ec_module_test . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\test_ec_module\!configuration!\ec_module_test.*"                              !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\tests\bin"
    set dest_default="!install_dir!\x64\Release\tests\default"
    set dest_scripts="!install_dir!\x64\Release\tests\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeAllDirs
    call :copyFile "!build_dir!\test_ec_module\!configuration!\ec_module_test.*"                                !dest_bin!

    )

goto :endproc



rem =============================
rem === POST_BUILD_EC_MODULE_DLL
rem =============================
:ec_module_dll
    echo "postbuild ec_module_dll . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\ec_module\!configuration!\ec_module_dll.*"                              !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_share=!install_dir!\x64\Release\share\bin
    call :makeDir "!dest_share!"

    call :copyFile "!build_dir!\ec_module\!configuration!\ec_module_dll.dll"                                "!dest_share!\ec_module.dll"

    )

goto :endproc



rem ===================================
rem === POST_BUILD_TEST_DELTARES_COMMON
rem ===================================
:test_deltares_common
    echo "postbuild test_deltares_common . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\test_deltares_common\!configuration!\test_deltares_common.*"                              !dest_bin!

  )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_bin="!install_dir!\x64\Release\tests\bin"
    set dest_default="!install_dir!\x64\Release\tests\default"
    set dest_scripts="!install_dir!\x64\Release\tests\scripts"
    set dest_plugins="!install_dir!\x64\Release\plugins\bin"
    set dest_share="!install_dir!\x64\Release\share\bin"

    call :makeAllDirs
    call :copyFile "!build_dir!\test_deltares_common\!configuration!\test_deltares_common.*"                                !dest_bin!

    )

goto :endproc



rem =============================
rem === POST_BUILD_IO_NETCDF_DLL
rem =============================
:io_netcdf_dll
    echo "postbuild io_netcdf_dll . . ."

    if "%configuration%" == "Debug" (

    echo "Debug postbuild"
    set dest_bin="!install_dir!\x64\Debug"

    call :makeDir !dest_bin!

    call :copyFile "!build_dir!\io_netcdf\!configuration!\io_netcdf_dll.*"                              !dest_bin!

    )

    if "%configuration%" == "Release" (

    echo "Release postbuild"

    set dest_share=!install_dir!\x64\Release\share\bin
    call :makeDir "!dest_share!"

    call :copyFile "!build_dir!\io_netcdf\!configuration!\io_netcdf_dll.dll"                                "!dest_share!\io_netcdf.dll"

    )

goto :endproc



rem =============================
rem === POST_BUILD_NEFIS_DLL
rem =============================
:nefis_dll
    echo "postbuild nefis_dll . . ."
    if "%configuration%" == "Debug" (
        echo "Debug postbuild"
        set dest_bin="!install_dir!\x64\Debug"
        call :makeDir !dest_bin!
        call :copyFile "!build_dir!\nefis\!configuration!\nefis_dll.*"               !dest_bin!
    )
    if "%configuration%" == "Release" (
        echo "Release postbuild"
        set dest_share=!install_dir!\x64\Release\share\bin
        call :makeDir "!dest_share!"
        call :copyFile "!build_dir!\nefis\!configuration!\nefis_dll.dll"             "!dest_share!\nefis.dll"
    )
goto :endproc









:end
echo oss-post_build.cmd finished
if NOT %ErrorLevel% EQU 0 (
   rem
      rem Only jump to :end when the script is completely finished
      rem
      exit %ErrorLevel%
  )

:endproc
   rem
   rem No exit here
   rem Otherwise the script exits directly at the first missing artefact
