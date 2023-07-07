#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs two Delft3D-FLOW instances on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run two Delft3D-FLOW instances on Linux."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-wconfig config_d_hydro_sed.xml"
    echo "       config file for water phase, default config_d_hydro_sed.xml"
    echo "-mconfig config_d_hydro_mud.xml"
    echo "       config file for mud   phase, default config_d_hydro_mud.xml"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
waterconfigfile=config_d_hydro.xml
mudconfigfile=config_d_hydro_mud.xml
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
    key="$1"
    shift

    case $key in
        -h|--help)
        print_usage_info
        ;;
        -mconfig)
        mudconfigfile="$1"
        shift
        ;;
        -wconfig)
        waterconfigfile="$1"
        shift
        ;;
        --D3D_HOME)
        D3D_HOME="$1"
        shift
        ;;
        --NNODES)
        NNODES="$1"
        shift
        ;;
    esac
done


if [ ! -f $waterconfigfile ]; then
    echo "ERROR: waterconfigfile $waterconfigfile does not exist"
    print_usage_info
fi
if [ ! -f $mudconfigfile ]; then
    echo "ERROR: mudconfigfile $mudconfigfile does not exist"
    print_usage_info
fi


workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    D3D_HOME=$scriptdir/..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/bin/.."
    # Remove "/.." at the end of the string
    scriptdir=${D3D_HOME%"/.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME


# On Deltares systems only:
if [ -f "/opt/apps/deltares/.nl" ]; then
    # Try the following module load
    module load intelmpi/21.2.0 &>/dev/null

    # If not defined yet: Define I_MPI_FABRICS and FI_PROVIDER with proper values for Deltares systems
    [ ! -z "$I_MPI_FABRICS" ] && echo "I_MPI_FABRICS is already defined" || export I_MPI_FABRICS=shm
    [ ! -z "$FI_PROVIDER" ] && echo "FI_PROVIDER is already defined" || export FI_PROVIDER=tcp
fi


echo "    Water config file: $waterconfigfile"
echo "    Mud   config file: $mudconfigfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
echo "    `type mpiexec`"
echo "    FI_PROVIDER      : $FI_PROVIDER"
echo "    I_MPI_FABRICS    : $I_MPI_FABRICS"
echo 

    #
    # Set the directories containing the binaries
    #

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH

    # Create shared memory block
esmContext=`$bindir/esm_create 2> /dev/null`
if [ $? -eq 0 ]; then
    # use context
    DIO_SHM_ESM=$esmContext
    export DIO_SHM_ESM
    echo "ESM context created: $esmContext"
else
    echo "Cannot create ESM context:"
    $bindir/esm_create
fi 

    echo "executing in background:"
    echo "$bindir/d_hydro $waterconfigfile &"
    echo 
$bindir/d_hydro $waterconfigfile &

echo waiting 5 seconds
sleep 5

    echo "executing in foreground:"
    echo "$bindir/d_hydro $mudconfigfile"
    echo 
$bindir/d_hydro $mudconfigfile


    # Wait until all child processes are finished
wait

$bindir/esm_delete $esmContext 2> /dev/null
if [ $? -ne 0 ]; then
    echo "Cannot delete ESM context $esmContext:"
    $bidir/esm_delete $esmContext
else
    echo "ESM context deleted: $esmContext"
fi 

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
