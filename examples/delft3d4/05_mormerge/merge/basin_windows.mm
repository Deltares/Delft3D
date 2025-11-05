# At present, this runscript will only work with build_configuration = build_all
# For this to work, the build.bat script in the top folder of the source tree needs to be executed for the "all (build full OSS tree)" configuration
# with automatic build, i.e. with the following option switched OFF: "Prepare only, no automatic compilation"
flowexedir       = ..\..\..\build_delft3d4\x64\dflow2d3d\bin
flowargs         = config_d_hydro.xml
waveexedir       = ..\..\..\build_delft3d4\x64\dwaves\bin
waveargs         = bas.mdw 1
swanbatdir       = ..\..\..\build_delft3d4\x64\swan\scripts
mormergeexedir   = ..\..\..\build_delft3d4\x64\dmor\bin
nodes            = local
# nodes            = 1
debug            = 1
# workdir          = e:\temp
condition:weight =   0deg : 3.0
condition:weight =  45deg : 1.0
# condition:weight =  90deg : 3.0
# condition:weight = 135deg : 1.0
# condition:weight = 180deg : 3.0
# condition:weight = 270deg : 3.0
