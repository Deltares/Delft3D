# At present, this runscript will only work with build_configuration = build_all
# For this to work, the build.sh script in the top folder of the source tree needs to be executed for the "all" configuration
# i.e. by executing ./build.sh all
flowexedir       = ../../../build_delft3d4/install/bin
flowargs         = config_d_hydro.xml
waveexedir       = ../../../build_delft3d4/install/bin
waveargs         = bas.mdw 1
swanbatdir       = ../../../build_delft3d4/install/bin
mormergeexedir   = ../../../build_delft3d4/install/bin
nodes            = local
# nodes            = 1
debug            = 0
# workdir          = /temp
condition:weight =   0deg : 3.0
condition:weight =  45deg : 1.0
# condition:weight =  90deg : 3.0
# condition:weight = 135deg : 1.0
# condition:weight = 180deg : 3.0
# condition:weight = 270deg : 3.0

