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
