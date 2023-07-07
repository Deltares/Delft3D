Debugging a coupled D-Flow FM - D-Waves computation in Visual Studio
====================================================================

Below: replace "<root>" by the location of the source code on your machine, e.g. "c:\checkouts\oss"

0. Run prepare_sln.py
1. Compile Release version of D-Waves
   This ensures that SWAN and ESMF_regridder are in the correct location: <root>\src\bin\x64
2. Compile Debug version
   DIMR, D-Flow FM and D-Waves
3. DIMR is the StartUp Project, in properties->Debugging:
   a. Set workdir
   b. Set input file (dimr_config.xml)
   c. Environment: Add all relevant paths to environment parameter PATH. Syntax/example:
      PATH=<root>\src\engines_gpl\dimr\bin\x64\Debug;<root>\src\bin\x64\Debug\dflowfm\bin;<root>\src\engines_gpl\wave\bin\x64\Debug;<root>\src\bin\x64\swan\scripts;<root>\src\bin\x64\swan\bin;<root>\src\bin\x64\esmf\scripts;<root>\src\bin\x64\esmf\bin;%PATH%;<root>\src\bin\x64\share\bin
      Ending with "%PATH%;sharedir" gives the highest chance on success.
4. Start debugging
   Use "F11" to jump from DIMR to D-Flow FM/D-Waves. Put breakpoints at the (three) lines with "->dllinitialize" in "dimr_lib->src->dimr.cpp". After jumping into D-Flow FM/D-Waves with "F11", you can add more breakpoints.

Debugging a coupled Delft3D4 - D-Waves computation in Visual Studio
==================================================================

0. Run prepare_sln.py
1. Compile a Release version of d_hydro, flow2d3d and D-Waves
2. Compile a Debug version of d_hydro, flow2d3d and D-Waves
3. Copy dll files from src/bin/x64/flow2d3d/bin folder to the Debug folder of the flow2d3d project
4. Delete the flow2d3d.dll file from the src/bin/x64/flow2d3d/bin folder
   d_hydro is the StartUp Project, in properties->Debugging:
   a. Set modeldir as workdir
   b. Set input file (config_d_hydro.xml)
3. Add a <waitfile> statement to your model's config_d_hydro xml file under the <flow2D3D name="myNameFlow"> section
4. Adapt the runscript run_dflow2d3d_dwaves.bat, so that the paths refer to the debug versions of your flow and wave libraries
5. Run the runscript; in VS, attach the Debug solution to the d_hydro process; 
6. Set your breakpoints
7. Add the waitfile to the modeldirectory
8. Start debugging using F5, F10, F11 to jump around in the code.

Debugging a parallel coupled D-Flow FM - D-Waves computation in Visual Studio
=============================================================================
0. Run prepare_sln.py
1. Compile Release version of D-Waves
   This ensures that SWAN and ESMF_regridder are in the correct location: <root>\src\bin\x64
2. Compile Debug version
   DIMR, D-Flow FM and D-Waves
3. DIMR is the StartUp Project, in properties->Debugging:
   a. Set workdir as folder that contains both fm and wave model dirs
   b. Set Command to <root>\src\bin\x64\share\bin\mpiexec.exe 
   c. Set Command arguments to -n <numberofpartitions> -localonly <root>\src\engines_gpl\dimr\bin\x64\Debug\dimr.exe <input file (eg. dimr_config.xml)>
   d. Environment: Add all relevant paths to environment parameter PATH. Syntax/example:
      PATH=<root>\src\engines_gpl\dimr\bin\x64\Debug;<root>\src\bin\x64\Debug\dflowfm\bin;<root>\src\engines_gpl\wave\bin\x64\Debug;<root>\src\bin\x64\swan\scripts;<root>\src\bin\x64\swan\bin;<root>\src\bin\x64\esmf\scripts;<root>\src\bin\x64\esmf\bin;%PATH%;<root>\src\bin\x64\share\bin
      Ending with "%PATH%;sharedir" gives the highest chance on success.
4. Add a <waitFile>waitforme.txt (or whatever)</waitFile> statement before the <control> block in dimr_config.xml (or whatever you named the control file)	  
5. Start debugging (F5)
   Use Debug->Attach to process to attach VS to the <numberofpartitions> dimr processes (dimr.exe)
   Add the file waitforme.txt to the dimr_config.xml directory
   Put breakpoints at the (three) lines with "->dllinitialize" in "dimr_lib->src->dimr.cpp". 
   Use "F11" to jump from DIMR to D-Flow FM/D-Waves. 
   After jumping into D-Flow FM/D-Waves with "F11", you can add more breakpoints.
   
Adri.Mourits@deltares.nl
Johan.Reyns@deltares.nl   
