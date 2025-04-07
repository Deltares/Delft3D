#!/bin/bash

ulimit -s unlimited

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]... [--] [DFLOWFMOPTIONS]..."
    echo "Run dflowfm program."
    echo
    echo
    echo "Options for ${0##*/}:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo
    echo "-p|--processlibrary <proc_def>"
    echo "       use an alternative process library file instead of $D3D_HOME/share/delft3d/proc_def"
    echo
    echo "-eco|--bloomspecies [<bloom.spe>]"
    echo "       use BLOOM, optionally using an alternative algea database for the default $D3D_HOME/share/delft3d/bloom.spe"
    echo
    echo "--"
    echo "       All following arguments are passed to D-Flow FM."
    echo "       (optional)"
    echo "Options for D-Flow FM, see dflowfm --help"

    exit 1
}

# Estimates the number of physical cores based on /proc/cpuinfo (not counting hyperthreading)
# If not found variable $numcores remains empty.
function estimate_num_cores {
    corespercpu=$(grep 'cpu cores' /proc/cpuinfo 2>/dev/null | head -1 | sed -e 's/.*:[ ]*\([0-9]*\)/\1/')
    numcpu=$(grep 'physical id' /proc/cpuinfo 2>/dev/null | sort | uniq | wc -l)

    if [ ! -z "$corespercpu" ] && [ ! -z "$numcpu" ]; then
        numcores=$(( ${numcpu}*${corespercpu} ))
    fi
}

function set_omp_threads {
    estimate_num_cores

    if [ ! -z "${OMP_NUM_THREADS}" ]; then
        # Do nothing: someone has already set OMP_NUM_THREADS
        :
    #elif [ ! -z "$numcores" ]; then
        # Use numcores-1 (if multiple cores at all)
        # Set OMP_NUM_THREADS variable in current process (no need to export)
    #    OMP_NUM_THREADS=$((numcores > 2 ? numcores-1 : 1))
    else
        # Could not determine numcores, leave OMP_NUM_THREADS empty.
        :
    fi
}

# Analyse the options

while [[ $# -ge 1 ]]
do
key="$1"
shift
case $key in
    -h|--help)
    print_usage_info
    ;;
    -p|--processlibrary)
    userprocfile="$1"
    shift
    ;;
    -eco|--bloomspecies)
    eco=true
    if [[ $# -ge 1 ]]
        then
        userspefile="$1" ## using only -eco would result in using the default spe-file in $D3D_HOME/share/delft3d/
        shift
    else
        userspefile=none
    fi
    ;;
    *)
    dfmoptions="$dfmoptions $key"
    ;;
esac
done

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
export D3D_HOME=$scriptdir/..
export LD_LIBRARY_PATH=$D3D_HOME/lib:$LD_LIBRARY_PATH


# On Deltares systems only:
if [ -f "/opt/apps/deltares/.nl" ]; then
    # Try the following module load
    module load intelmpi/21.2.0 &>/dev/null

    # If not defined yet: Define I_MPI_FABRICS and FI_PROVIDER with proper values for Deltares systems
    [ ! -z "$I_MPI_FABRICS" ] && echo "I_MPI_FABRICS is already defined" || export I_MPI_FABRICS=shm
    [ ! -z "$FI_PROVIDER" ] && echo "FI_PROVIDER is already defined" || export FI_PROVIDER=tcp
fi


# Fill in default options

if [ ! "$userprocfile" == "" ]
    then
    procfile=$userprocfile
else
    procfile=$D3D_HOME/share/delft3d/proc_def.dat
fi

if [ ! -f $procfile ]; then
    if [ ! -f $procfile.dat ]; then
        echo "ERROR: procfile $procfile does not exist"
        print_usage_info
    fi
fi

spefile=$D3D_HOME/share/delft3d/bloom.spe
if [ "$eco" == "true" ]
   then
   if [ ! -f $userspefile ]; then
       if [ ! -f $spefile ]; then
          echo "ERROR: default bloom.spe $spefile does not exist"
          echo "ERROR: the optional specified bloom.spe $userspefile does not exist either"
          print_usage_info
       else
          echo "Using default bloom.spe"
       fi
   else
       echo "Using specified bloom.spe $userspefile"
       spefile=$userspefile
   fi
fi

set_omp_threads

echo "$D3D_HOME/bin/dflowfm --nodisplay --autostartstop --processlibrary $procfile --bloomspecies $spefile $dfmoptions"
#ldd $D3D_HOME/bin/dflowfm
$D3D_HOME/bin/dflowfm --nodisplay --autostartstop --processlibrary $procfile --bloomspecies $spefile $dfmoptions


