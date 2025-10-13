#!/bin/bash
    #
    # This script is an example for running Delft3D-FLOW
    # Adapt and use it for your own purpose
    #
    # adri.mourits@deltares.nl
    # 27 Dec 2010
    # 
    #
    # This script starts a single-domain Delft3D-FLOW computation on Linux
    #


    #
    # Set the config file here
    # 
argfile=config_d_hydro.xml





    #
    # Set the directory containing delftflow.exe here
    #
export ARCH=lnx64
export D3D_HOME=../../../bin
exedir=$D3D_HOME/$ARCH/flow2d3d/bin
 


rm -f trim*.*
rm -f tri-diag.*
rm -f TMP_*.*
rm -f tstprt.*
rm -f COSUMO/FF2NF/FF2NF*.txt

    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 

    # Run
$exedir/d_hydro.exe $argfile
