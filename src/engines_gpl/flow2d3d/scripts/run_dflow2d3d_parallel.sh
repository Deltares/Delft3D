#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW in parallel on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} n [OPTION]..."
    echo "Run a Delft3D-FLOW model in parallel on Linux."
    echo
    echo "n: integer, number of partitions"

    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "<filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
NPART=0
configfile=config_d_hydro.xml
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

NPART="$1"
shift
while [[ $# -ge 1 ]]
do
    key="$1"
    shift

    case $key in
        -h|--help)
        print_usage_info
        ;;
        --D3D_HOME)
        D3D_HOME="$1"
        shift
        ;;
        --NNODES)
        NNODES="$1"
        shift
        ;;
        *)
        configfile="$key"
        ;;
    esac
done


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
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
    # Scriptdir: remove "/.." at the end of the string
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


echo "    Configfile       : $configfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
echo "    nr of parts      : $NPART"
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

export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
export PATH="$bindir:${PATH}"


# Run
echo "executing:"
echo "mpirun -np $NPART $bindir/d_hydro $configfile"
      mpirun -np $NPART $bindir/d_hydro $configfile
echo 



    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
