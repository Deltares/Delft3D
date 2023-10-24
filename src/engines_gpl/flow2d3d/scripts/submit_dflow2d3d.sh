#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW in parallel on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # Execute in the working directory:
    # /path/to/delft3d/installation/lnx64/bin/submit_dflow2d3d.sh
    # More examples: check run scripts in https://git.deltares.nl/oss/delft3d/-/tree/main/examples/*

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run a Delft3D-FLOW model in parallel on Linux."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of partitions per node, default $corespernodedefault"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-j, --jobname <jobname>"
    echo "       jobname prefix, default Delft3D4-FLOW"
    echo "-m, --masterfile <filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    echo "-n, --numnode <N>"
    echo "       number of nodes, default 1"
    echo "-q, --queue <qname>"
    echo "       queue, default normal-e3-c7"
    echo "--rtc"
    echo "       Online with RTC. Not possible with parallel Delft3D-FLOW."
    echo "-w, --wavefile <wname>"
    echo "       name of mdw file"
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
JOBNAME=Delft3D4-FLOW
numnode=1
queue=normal-e3-c7
runscript_extraopts=
wavefile=runwithoutwaveonlinebydefault
withrtc=false


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
        -n|--numnode)
        numnode="$1"
        shift
        ;;
        -q|--queue)
        queue="$1"
        shift
        ;;
        --rtc)
        withrtc=true
        shift
        ;;
        -w|--wavefile)
        wavefile="$1"
        shift
        ;;
        -j|--jobname)
        JOBNAME="$1"
        shift
        ;;
        -m|--masterfile)
        configfile="$1"
        shift
        ;;
        --)
        echo "-- sign detected, remained options are going to be passed to dimr"
        runscript_extraopts="$runscript_extraopts $*"
        break       # exit loop, stop shifting, all remaining arguments without dashes handled below
        ;;
        -*)
        echo "option ${key} seems dedicated for dimr, therefore passing it and the following ones to the dimr"
        runscript_extraopts="$key $*"
        break       # exit loop, $key+all remaining options to dflowfm executable
        ;;
    esac
done


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi


workdir=`pwd`

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
D3D_HOME=$scriptdir/..
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
RUNSCRIPT=$scriptdir/rd2d3d.sh

JOBNAME="${JOBNAME}_${numnode}x${corespernode}"

echo "    Configfile                : $configfile"
echo "    D3D_HOME                  : $D3D_HOME"
echo "    Working directory         : $workdir"
echo "    nr of partitions          : $numnode"
echo "    nr of partitions per node : $corespernode"
echo "    Queue                     : $queue"
echo "    Job name                  : $JOBNAME"
echo

    #
    # Set the directories containing the binaries
    #


runscript_opts="-m ${configfile} -c $corespernode --NNODES $numnode --D3D_HOME ${D3D_HOME}"
if [ "$wavefile" != "runwithoutwaveonlinebydefault" ]; then
    runscript_opts="$runscript_opts -w ${wavefile}"
fi
if $withrtc ; then
    runscript_opts="$runscript_opts --rtc"
fi
runscript_opts="$runscript_opts $runscript_extraopts"
if [ $numnode == 1 ]; then
    echo "qsub -q $queue  -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}"
          qsub -q $queue  -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}
else
    echo "qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}"
          qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}
fi
