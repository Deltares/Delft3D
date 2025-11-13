# Compiling Delft3D on Windows

## Prerequisites
- [Microsoft Visual Studio 2022 Community Edition](https://visualstudio.microsoft.com/vs/community/) and make sure to include C++/CLI support, C++ MFC, and Latest Windows 11 SDK.
- Alternatively, you may consider using [Visual Studio Code](https://code.visualstudio.com/).
- [Intel oneAPI Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html) Please make sure that it's integrated into the Visual Studio environment installed above.
- [Intel oneAPI MPI Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html)
- [Intel oneAPI Math Kernal Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html)
- [CMake](https://cmake.org/download/) version 3.30 or later
- [Git](https://gitforwindows.org/)
- For more advanced development steps, such as running test benches, you will need a Python installation.

## Build steps
- Download or clone the source code from https://github.com/Deltares/Delft3D
- Execute `build.bat` **from an Intel oneAPI command prompt for Intel 64 for Visual Studio 2022**.
  Execute `build.bat -help` to show the usage.
  This step uses CMake to create the Visual Studio build environment.
  By default, it creates the build environment for the Delft3D FM Suite (`fm-suite`).
- Open the generated solution from the command prompt to ensure that the intel environment is inherited by visual studio. For example:
  `devenv build_fm-suite\fm-suite.sln`
- Build from Visual Studio, or alternatively, use the command line to run

   `cmake --build build_fm-suite --config Debug`

   `cmake --install build_fm-suite --config Debug`

## Alternative: without build-script
Refer to the [README](src\cmake\README) file in the CMake folder.
WARNING: When building without our build-script, the collection of the resulting binaries will need attention

