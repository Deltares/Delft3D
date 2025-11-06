# Compiling Delft3D

## Windows
- build.bat from an Intel oneAPI command prompt for Intel 64 for Visual Studio 2022.
  Execute "build.bat -help" to show the usage.
- Open the generated solution from the command prompt to ensure that the intel environment is inherited by visual studio. For example:
  "devenv build_fm-suite\fm-suite.sln"
- Build from visual studio, or alternatively, use the command line to run
  "cmake --build build_fm-suite --config Debug"
  "cmake --install build_fm-suite --config Debug"

## Linux
- build.sh
  Execute "./build.sh --help" to show the usage
  Currently used as default build process: "./build.sh fm-suite --compiler intel21"
  This will execute "src/setenv.sh" on Deltares systems. On other systems, the environment must be prepared upfront.
  For instructions, see [Setup your own Linux environment](Linux_setup.md).

#### Alternative: without build-script (Windows and Linux)
See ...\src\cmake\README
WARNING: When building without build-script, the collection of the resulting binaries will need attention

#### More information:
- Delft3D FM suite: https://oss.deltares.nl/web/delft3dfm/get-started
- Delft3D 4  suite: https://oss.deltares.nl/web/delft3d/get-started
