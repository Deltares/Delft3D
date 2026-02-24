# Compiling Delft3D on Windows

## Prerequisites
- Microsoft Visual Studio (latest version tested version 2022), this can be the [Community Edition](https://visualstudio.microsoft.com/vs/community/).
  Choose the **"Desktop development with C++"** configuration.
  Make sure to include under the list of installation details on the right side of the installation dialog the items "C++/CLI support", "C++ MFC", and the latest "Windows SDK".
  <img src="images/VisualStudio_installed_components.png">
  https://www.junian.net/dev/visual-studio-community-download-links/
- You may use [Visual Studio Code](https://code.visualstudio.com/) as the development environment, but a Visual Studio installation is still required for the C++ compiler and the Intel Fortran installation.
- [Intel oneAPI Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html) Please make sure that it's integrated into the Visual Studio environment installed above.
- [Intel oneAPI MPI Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html)
- [Intel oneAPI Math Kernal Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html)
- [CMake](https://cmake.org/download/) for configuring the build environment
- [Git](https://gitforwindows.org/) for downloading the Delft3D source code from this repository.
If you prefer user interfaces over command lines, you may want to additionally install [GitExtensions](https://gitextensions.github.io/) and/or [TortoiseGit](https://tortoisegit.org/).
The former tool is generally considered more powerful and true to Git.
The latter tool offers icon overlays for commit status.
- [Python](https://www.python.org/downloads/) for some build steps

## Build steps
- Download or clone the source code from https://github.com/Deltares/Delft3D

   `git clone https://github.com/Deltares/Delft3D.git`

- Execute `build.bat` from an **Intel oneAPI command prompt for Intel 64 for Visual Studio XXX** (where XXX depends on the version installed; you can find this command prompt in your Windows Start menu).
  Execute `build.bat -help` to show the supported command line options.
  This step uses CMake to create the Visual Studio build environment.
  By default, it creates the build environment for the Delft3D FM Suite (`fm-suite`) but you can also build environments for the Delft3D 4 Suite (`d3d4-suite`) and everything (`all`) by changing the selected configuration (see the command line options).
- Experienced developers may prefer to trigger the CMake build system manually by means of a command such as

   `cmake -S .\src\cmake -B build_fm-suite  -T fortran=ifx -A x64 -D CONFIGURATION_TYPE:STRING="fm-suite" -D CMAKE_INSTALL_PREFIX=.\install_fm-suite\`
  
  instead of using the `build.bat` script.
- Open the generated solution from the **Intel oneAPI command prompt for Intel 64 for Visual Studio XXX** command prompt to ensure that the intel environment is inherited by visual studio. For example:
  `devenv build_fm-suite\fm-suite.sln`
  or
  `devenv build_fm-suite\fm-suite.slnx`
  for Visual Studio 2026.
  Most compilation steps work fine when the solution is opened outside the Intel oneAPI environment, but you will see some MSB3073 errors with a description `The command "setlocal`.
  Those errors are related to the collection step for the GoogleTest framework.
- Build from Visual Studio, or alternatively, use the command line to run

   `cmake --build build_fm-suite --config Debug`

   `cmake --install build_fm-suite --config Debug`
