#! /bin/bash  
# Specify Slurm SBATCH directives 
#SBATCH --nodes=1              # Number of nodes.
#SBATCH --ntasks-per-node=1     # The number of tasks to be invoked on each node.
                                # For sequential runs, the number of tasks should be '1'.
                                # Note: SLURM_NTASKS is equal to "--nodes" multiplied by "--ntasks-per-node".
#SBATCH --job-name=delft3d4     # Specify a name for the job allocation.
#SBATCH --time=00:15:00         # Set a limit on the total run time of the job allocation.
#SBATCH --partition=4vcpu       # Request a specific partition for the resource allocation.
                                # See: https://publicwiki.deltares.nl/display/Deltareken/Compute+nodes.

set -eo pipefail

function print_usage_info {
    echo "Usage: sbatch [SLURM OPTIONS]... ${0##*/} [OPTION]..."
    echo "Run delft3d4 on H7."
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
    echo "The following arguments are used when called by submit_dflow2d3d_h7.sh:"
    echo "--D3D_HOME <path>"
    echo "       path to binaries and scripts"
    echo "    --NODES <N>"
    echo "       number of partitions=NODES*CoresPerNode, default 1 (not parallel)"
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
NSLOTS=$SLURM_TASKS_PER_NODE
debuglevel=-1
runscript_extraopts=
wavefile=runwithoutwaveonlinebydefault
withrtc=0
csumoscript=/p/1202339-rndcoastalhd/COSUMO/01_code/exe/latest/lnx_h7_2023b/run_COSUMO.sh # latest compiled version of C-SUMO
mcrdir=/p/1202339-rndcoastalhd/COSUMO/10_MCR_linux/h7/2023b/installed/R2023b/  # Matlab Runtime compiler location (should be consistent with C-SUMO executable)
csumodeployed=true
csumodir=/p/1202339-rndcoastalhd/COSUMO/01_code/
matlabversion=2014a


ulimit -s unlimited
export I_MPI_FABRICS=ofi
export FI_PROVIDER=tcp
export I_MPI_OFI_PROVIDER=tcp
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

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
    echo " Wavefile     "
    wavefile="$1"
    echo $wavefile
    shift
    ;;
    -csumoscript)
    csumoscript="$1"
    shift
    ;;
    -mcrdir)
    mcrdir="$1"
    shift
    ;;
    --D3D_HOME)
    D3D_HOME="$1"
    shift
    ;;
    -csumodeployed)
    csumodeployed="$1"
    shift
    ;;
    -csumodir)
    csumodir="$1"
    shift
    ;;
    -matlabversion)
    matlabversion="$1"
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

# ------------- COSUMO related stuff -----------------------

echo "Preparing COSUMOsettings.xml"

# remove any old COSUMOsettings.xml file
rm -f COSUMOsettings.xml

# export paths
export rundir=$(pwd)
export reldir=${PWD##*/}

export ff2nffolder=$rundir/FF2NFdir/
# export ff2nffolder=/p/1202339-rndcoastalhd/COSUMO/09_runscripts_d3d_fm/Delft3D4/parallel/h6_c7/C-sumo_on_node/testruns/C01/FF2NFdir/

# copy COSUMO_template_settings.xml to COSUMOsettings.xml
echo "	copying COSUMO_template_settings.xml to COSUMOsettings.xml"
cp COSUMO_template_settings.xml COSUMOsettings.xml

# replace keywords COSUMOsettings.xml
export find1=%FF2NFDIR%
export replace1=$ff2nffolder

echo "	replacing %FF2NFDIR% in COSUMOsettings.xml with $ff2nffolder"
sed -i "s,$find1,$replace1," COSUMOsettings.xml
# ------------------------------------------------------------

# Check configfile    
if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi

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

echo "    Configfile           : $configfile"
echo "    D3D_HOME             : $D3D_HOME"
echo "    Working directory    : $workdir"
echo "    nr of tasks per node : $SLURM_TASKS_PER_NODE"
echo "    Number of partitions : $NSLOTS"
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

echo "--------------------------------------------------------------------------------"
echo "-----------------------Starting C-SUMO in the background------------------------"
echo "	create FF2NFdir folder (if it does not exist)"
echo "		FF2NFdir = $ff2nffolder" 
mkdir -p -- "$ff2nffolder"

if $csumodeployed ; then
	echo "	Starting C-SUMO in deployed mode using this command:"
	echo "		$csumoscript $mcrdir $ff2nffolder 0 &"
	$csumoscript $mcrdir $ff2nffolder 0 &
    csumo_pid=$!
    echo "    The PID of the C-SUMO process is $csumo_pid"
else
	echo "  Starting up a MATLAB instance to run C-SUMO"
	echo "		cd /opt/apps/matlab/$matlabversion/bin/"
	cd /opt/apps/matlab/$matlabversion/bin/
	echo "		matlab -nodisplay -nosplash -nodesktop -r cd('$csumodir');COSUMO('$ff2nffolder',0); &"
	./matlab -nodisplay -nosplash -nodesktop -r "cd('$csumodir');COSUMO('$ff2nffolder',0);" &
	cd $rundir
    csumo_pid=$!
    echo "    The PID of the C-SUMO process is $csumo_pid"
fi

# Give C-SUMO some time to start before Delft3D-FLOW starts
sleep 10

echo "--------------------------------------------------------------------------------"



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
        module load intelmpi/2021.11.0 &>/dev/null
        echo ----------------------------------------------------------------------
        echo "srun $bindir/d_hydro $configfile"
              srun $bindir/d_hydro $configfile
    fi
fi

# kill C-SUMO
echo "kill C-SUMO using the following command: kill -9 $csumo_pid"
kill -9 $csumo_pid

    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
