# Dummy PreCICE coupler that sends a greeting
This dummy coupler receives a mesh from the other component and writes a greeting message in floating-point encoded ascii values.

## How to build
> cmake -S greeter_dummy -B build_greeter_dummy_debug -D CMAKE_BUILD_TYPE=Debug
> cmake --build build_greeter_dummy_debug

## How to run
Create a directory next to precice_config.xml and use it as the working directory from which greeter_dummy is run.
For example, place build_greeter_dummy_debug on the PATH, create a greeter_dummy folder next to precice_config.xml
and start up greeter_dummy from there. The reason is that dimr sets the working directories to the component directory
in a model folder, and if the precice_config.xml lives next to the dimr_config.xml, then this is one directory down.
Further, the exchange-directory specified in precice_config.xml is shared between all components, and if this is a
relative path (e.g., '..') then this path should be shared between all components.

greeter_dummy expects one command line argument that contains the name of the mesh it receives from the other component.

Start up the other component and the greeter_dummy in separate shells that run in the same container, so that they can
communicate over the network. In VSCode, simply split the terminal. Running docker from the command line,
use 'docker ps' to get the container ID and then run 'docker exec -it <container ID> bash'. Then run the two executables, for example,
/checkouts/delft3d/examples/dflowfm/01_dflowfm_sequential               > dimr dimr_config.xml
/checkouts/delft3d/examples/dflowfm/01_dflowfm_sequential/greeter_dummy > greeter_dummy fm-mesh
