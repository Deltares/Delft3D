# Tarball
Please use the tarball containing the latest released version of the source code, located at:
https://oss.deltares.nl/en/web/delft3dfm/get-started#Download%20source%20code
See section "Workflow" below in case you want to contribute to the source code.



# About compiling https://git.deltares.nl/oss/delft3d

#### Windows:
- build.bat
  Execute "build.bat --help" to show the usage
  Currently used as default build process: "build.bat fm-suite -vs 2019 -ifort 21"
  This will execute "Microsoft_VisualStudio\vcvarsall.bat". When using other versions, modifications will be needed.

#### Linux:
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



# Debugging DIMR in VisualStudio

Note: in this section:
Replace "..." by the actual path on your system to the checkout directory.

- Use build.bat to prepare the "fm-suite" configuration
- Open "...\build_fm-suite\fm-suite.sln" in VisualStudio and build the complete release version
  Directory "...\build_fm-suite\x64\Release\share\bin" will be created
- Build the debug versie of what you need (e.g. dimr and dflowfm, waq, wave)
- dimr project -> Set as Startup Project
- dimr project -> properties -> Debugging:
    -> Command Arguments: dimr_config.xml
    -> Working Directory: ...\examples\12_dflowfm\test_data\e100_f02_c02-FriesianInlet_schematic_FM
    -> Environment: PATH=...\build_fm-suite\x64\Debug;%PATH%;...\fm-suite\x64\Release\share\bin



# Workflow

- Request for access on https://git.deltares.nl/oss/delft3d
- Create an issue in https://issuetracker.deltares.nl
  If an issue is not created, you have to create a branch of type research
- Clone the repository
- Create a branch using the naming convention below
  The frequency of updating your branch from main is up to personal taste.
  Yet, merge from main as often as possible, and merge back to main as early as possible.
- Create a MergeRequest (not for research branches):
  - TeamCity projects will be triggered to build the source code (Windows and Linux). Continuation is only possible when it succeeds. This will take at least 30 minutes.
  - A small set of QuickTests will be triggered on TeamCity. Continuation is only possible when it succeeds. This will take at least 30 minutes.
  - You have to assign the MergeRequest to a core developer for reviewing and testing. When succeeded, the tester/reviewer is allowed to merge into trunk.
- Official binary deliveries are only allowed using Deltares TeamCity server



# Branch naming

\<kernel\>/\<type\>/\<ISSUENR\>_short_description
with:
- \<kernel\>  : one of: all, d3d4, fm, none, part, rr, swan, waq, wave, tc
  -> Use all/none/\<specific\> to trigger all/none/specific tests
  -> Not needed for type \<research\>.
- \<type\>    : one of: bugfix, doc, feature, poc, release, research, task
  -> Use \<research\> for branches that will not be merged into trunk directly.
- \<ISSUENR\> : JIRA issue number
  -> Not needed for type \<research\>.

Examples:
- fm/feature/UNST-1234_improve_partition_file
- research/improve_flow_scheme

# Unit tests
## Running Unit tests
- After building the source code, execute "ctest" in the build directory
- Then run ctest followed by the config

```
  cd build_fm-suite
  ctest -C debug
```
- For more details about the unit testing utilities in cmake, see [Fortran Unit Testing](doc/unit-testing.md).
