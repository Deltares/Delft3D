#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # Execute in the working directory:
    # /path/to/delft3d/installation/lnx64/bin/submit_dflow2d3d.sh
    # More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "This script is called by submit_dflow2d3d.sh."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of partitions per node, default $corespernodedefault"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-m, --masterfile <filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    echo "-w, --wavefile <wname>"
    echo "       name of mdw file"
    echo "--rtc"
    echo "       Online with RTC. Not possible with parallel Delft3D-FLOW."
    echo "The following arguments are used when called by submit_dflow2d3d.sh:"
    echo "    --D3D_HOME <path>"
    echo "       path to binaries and scripts"
    echo "    --NNODES <N>"
    echo "       number of partitions=NNODES*CoresPerNode, default 1 (not parallel)"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
configfile=config_d_hydro.xml
corespernodedefault=1
corespernode=$corespernodedefault
D3D_HOME=
debuglevel=-1
runscript_extraopts=
NNODES=1
wavefile=runwithoutwaveonlinebydefault
withrtc=0


ulimit -s unlimited


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -c|--corespernode)
    corespernode=$1
    shift
    ;;
    -h|--help)
    print_usage_info
    ;;
    -m|--masterfile)
    configfile="$1"
    shift
    ;;
    --rtc)
    withrtc=1
    shift
    ;;
    -w|--wavefile)
    wavefile="$1"
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
    --)
    echo "-- sign detected, remained options are going to be passed to Delft3D-FLOW"
    runscript_extraopts="$runscript_extraopts $*"
    break       # exit loop, stop shifting, all remaining arguments without dashes handled below
    ;;
    -*)
    echo "option ${key} seems dedicated for Delft3D-FLOW, therefore passing it and the following ones to Delft3D-FLOW"
    runscript_extraopts="$key $*"
    break       # exit loop, $key+all remaining options to Delft3D-FLOW executable
    ;;
esac
done

# Check configfile
if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi

export NSLOTS=`expr $NNODES \* $corespernode`

workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    export D3D_HOME=$scriptdir/..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/bin/.."
    # To obtain scriptdir: remove "/.." at the end of the string
    scriptdir=${D3D_HOME%"/.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
if [[ $withrtc -ne 0 && $NSLOTS -ne 1 ]] ; then
    echo "ERROR: Combination of RTC online and Delft3D-FLOW in parallel is not implemented yet"
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


echo "    Configfile           : $configfile"
echo "    D3D_HOME             : $D3D_HOME"
echo "    Working directory    : $workdir"
echo "    Number of partitions : $NSLOTS"
echo "    `type mpiexec`"
echo "    FI_PROVIDER          : $FI_PROVIDER"
echo "    I_MPI_FABRICS        : $I_MPI_FABRICS"
if [ "$wavefile" != "runwithoutwaveonlinebydefault" ]; then
    echo "    Wave file            : $wavefile"
fi
if [ $withrtc -ne 0 ] ; then
    echo "    Online with RTC      : YES"
fi


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
# export LD_PRELOAD=$libdir/libmkl_core.so

# For debugging only
if [ $debuglevel -eq 0 ]; then
    echo === LD_LIBRARY_PATH =========================================
       echo $LD_LIBRARY_PATH
    echo =========================================================
    echo " "
    echo === ldd $libdir/libflow2d3d.so =========================================
             ldd $libdir/libflow2d3d.so
    echo =========================================================
    echo " "
    echo ===  ldd $bindir/d_hydro =========================================
              ldd $bindir/d_hydro
    echo ========================================================
fi

if [ $withrtc -ne 0 ] ; then
    #
    #
    # Separate block when running with RTC online
    #
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
    $bindir/rtc $sharedir/rtc/RTC.FNM $workdir/RTC.RTN

    # Remove allocated shared memory
    $bindir/esm_delete $DIO_SHM_ESM



else
    #
    #
    # Without RTC online
    #
    # Optionally, start D-Waves in the background
    if [ "$wavefile" != "runwithoutwaveonlinebydefault" ]; then
        if [ ! -f $wavefile ]; then
            echo "ERROR: Wave input file $wavefile does not exist"
            print_usage_info
        fi
        echo "executing in the background:"
        echo "$bindir/wave $wavefile 1 &"
              $bindir/wave $wavefile 1 &
    fi

    if [ $NSLOTS -eq 1 ]; then
        echo "executing:"
        echo "$bindir/d_hydro $configfile"
              $bindir/d_hydro $configfile
    else
        #
        # Create machinefile using $PE_HOSTFILE
        if [ $NNODES -eq 1 ]; then
            echo " ">$(pwd)/machinefile
        else
            if [ -n $corespernode ]; then
                if [ -e $(pwd)/machinefile ]; then
                    rm -f machinefile
                fi
                for (( i = 1 ; i <= $corespernode; i++ )); do
                    awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
                done
            else
               awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
            fi
        fi
        echo Contents of machinefile:
        cat $(pwd)/machinefile
        echo ----------------------------------------------------------------------
        echo "mpiexec -np $NSLOTS $bindir/d_hydro $configfile"
              mpiexec -np $NSLOTS $bindir/d_hydro $configfile
    fi
fi


    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
