#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script is an example for running dimr on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # qsub run_dimr.sh
    #
    # adri.mourits@deltares.nl
    # 20 Oct 2016
    #

    #
    # Set the config file
    #
argfile=dimr_config.xml

    #
    # Set the directories containing the executables
    #
export ARCH=lnx64
export D3D_HOME=../../bin/

dimrexedir=$D3D_HOME/$ARCH/dimr/bin
waqexedir=$D3D_HOME/$ARCH/waq/bin
export proc_def_dir=../../bin/lnx64/waq/resources

    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$dimrexedir:$waqexedir:$LD_LIBRARY_PATH
export PATH=$PATH
# $dimrexedir/dimr.exe $argfile -d 0xFFFFFFFF
$dimrexedir/dimr.exe $argfile


    # Wait until all child processes are finished
wait
