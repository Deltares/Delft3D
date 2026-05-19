# Compiling Delft3D on Windows
Back to main [development page](development.md).

## Prerequisites
- Microsoft Visual Studio, this can be the Enterprise Edition, Professional Edition, or the [Community Edition](https://visualstudio.microsoft.com/vs/community/).
  During the installation/configuration process, choose the **"Desktop development with C++"** configuration.
  Make sure to include under the list of installation details on the right side of the installation dialog the items **"C++/CLI support"**, **"C++ MFC"**, and the latest **"Windows SDK"**; see the figure below.  
  ![List of installation details](images/VisualStudio_installed_components.png)
  
  Links to previous Visual Studio Community Editions can be found [here](https://www.junian.net/dev/visual-studio-community-download-links/).
  See the note below on the use of different versions.
- You may use [Visual Studio Code](https://code.visualstudio.com/) as the development environment, but a Visual Studio installation is still required for the C++ compiler and the Intel Fortran installation.
- [Intel oneAPI Fortran Compiler](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html) Please make sure that it's integrated into the Visual Studio environment installed above.
  See the note below on the use of different versions.
- [Intel oneAPI MPI Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html)
- [Intel oneAPI Math Kernel Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html)
- [CMake](https://cmake.org/download/) for configuring the build environment
- [Git](https://gitforwindows.org/) for downloading the Delft3D source code from this repository.
If you prefer user interfaces over command lines, you may want to additionally install [GitExtensions](https://gitextensions.github.io/) and/or [TortoiseGit](https://tortoisegit.org/).
The former tool is generally considered more powerful and true to Git.
The latter tool offers icon overlays for commit status.
- [Python](https://www.python.org/downloads/) for the build scripts and Conan package manager
- [Conan 2](https://docs.conan.io/2/installation.html) — install via `pip install conan`

**Note**
- We are currently using Visual Studio 2022 and Intel oneAPI 2024.2 for the official release.
  Visual Studio 2026 and Intel oneAPI 2025.3 were used successfully to build the software, but we have not yet thoroughly tested the resulting binaries.
  Since the Windows build includes third-party libraries that have been compiled using Visual Studio 2022 against Intel oneAPI 2024.2, runtime problems are possible.
  We aim to transition to the updated versions in the near future.


## One-time Conan setup

Before building for the first time, initialise the Conan configuration.
This installs compiler profiles, settings, and (for Deltares developers) the Nexus remote where pre-built binary packages are hosted.

**Deltares developers** (with Nexus access):
```
python run_conan.py --initialize-conan=deltares
```

**External / open-source developers** (without Nexus access):
```
python run_conan.py --initialize-conan=external
```

## Build steps
- Download or clone the source code from https://github.com/Deltares/Delft3D
  ```
  git clone https://github.com/Deltares/Delft3D.git
  ```
- Run `build.py` from an **Intel oneAPI command prompt for Intel 64 for Visual Studio XXX** (where XXX depends on the version installed; you can find this command prompt in your Windows Start menu).
  Execute `python build.py --help` to show the supported command line options.
  This step uses Conan to fetch third-party dependencies and CMake to create the Visual Studio build environment.
  By default, it creates the build environment for the Delft3D FM Suite (`fm-suite`) but you can also build environments for the Delft3D 4 Suite (`d3d4-suite`) and everything (`all`) by changing the selected configuration (see the command line options).
  ```
  python build.py --config fm-suite
  ```
  To also compile and install in one go:
  ```
  python build.py --config fm-suite --build
  ```

  **Open-source developers** without access to the Deltares Nexus must build all third-party
  dependencies from the local recipes by adding `--build-dependencies`:
  ```
  python build.py --config fm-suite --build --build-dependencies
  ```

- Open the generated solution from the **Intel oneAPI command prompt for Intel 64 for Visual Studio XXX** to ensure that the intel environment is inherited by visual studio. For example:
  ```
  devenv build_fm-suite\fm-suite.sln
  ```
  or
  ```
  devenv build_fm-suite\fm-suite.slnx
  ```
  for Visual Studio 2026.
  Most compilation steps work fine when the solution is opened outside the Intel oneAPI environment, but you will see some [MSB3073](https://learn.microsoft.com/en-us/visualstudio/msbuild/errors/msb3073) errors with a description starting with `The command "setlocal`.
  Those errors are related to the collection step for the GoogleTest framework.
- Build from Visual Studio, or alternatively, use the command line to run
  ```
  cmake --build build_fm-suite --config Debug 
  cmake --install build_fm-suite --config Debug
  ```
  to build the debug version of the Delft3D FM binaries.

## Power-user workflow (Conan + CMake separately)

If you prefer to drive Conan and CMake individually (e.g. when iterating on CMake changes without
re-running Conan):

```bat
:: 1. Install dependencies (generates CMakeDeps files in the output folder)
::    The profile was installed by --initialize-conan; the lockfile ensures reproducibility.
conan install . --profile:all=delft3d_windows --settings:all build_type=Release ^
      --output-folder=build_fm-suite/conan --lockfile=conan.lock

:: 2. CMake configure
cmake -S .\src\cmake -B build_fm-suite -T fortran=ifx -A x64 ^
      -D CONFIGURATION_TYPE:STRING="fm-suite" ^
      -D CMAKE_INSTALL_PREFIX=.\install_fm-suite

:: 3. Build & install
cmake --build build_fm-suite --config Debug --parallel
cmake --install build_fm-suite --config Debug
```

To build missing dependencies from source (e.g. after changing a recipe):
```bat
conan install . --profile:all=delft3d_windows --settings:all build_type=Release ^
      --output-folder=build_fm-suite/conan --build=missing
```
