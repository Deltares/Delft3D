#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run a Delft3D-FLOW model online with RTC on Linux."
    echo
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
configfile=config_d_hydro.xml
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
    # Scriptdir: Remove "/.." at the end of the string
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
echo "    `type mpiexec`"
echo "    FI_PROVIDER      : $FI_PROVIDER"
echo "    I_MPI_FABRICS    : $I_MPI_FABRICS"
echo 

    #
    # Set the directories containing the binaries
    #

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib
sharedir=$D3D_HOME/share


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
export PATH=$bindir:$PATH

# Shared memory allocation
export DIO_SHM_ESM=`$bindir/esm_create`
# Start Delft3D-FLOW in the background
echo "executing:"
echo "$bindir/d_hydro $configfile &"
      $bindir/d_hydro $configfile &

# Be sure Delft3D-FLOW is started before RTC is started
sleep 5
# echo press enter to continue
# read dummy

# Start RTC
echo "executing:"
echo "$bindir/rtc $sharedir/rtc/RTC.FNM $workdir/RTC.RTN"
      $bindir/rtc $sharedir/rtc/RTC.FNM $workdir/RTC.RTN

# Remove allocated shared memory
$bindir/esm_delete $DIO_SHM_ESM 

    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
