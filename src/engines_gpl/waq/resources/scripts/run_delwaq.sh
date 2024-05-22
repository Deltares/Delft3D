#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delwaq on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <delwaq.inp> [OPTION]..."
    echo "Run a Delwaq model on Linux."
    echo
    echo "<delwaq.inp>"
    echo "       Delwaq input file"
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo
    echo "-p <proc_def>"
    echo "       use an alternative process library file instead of $D3D_HOME/share/delft3d/proc_def"
    echo
    echo "-np"
    echo "       do not use any Delwaq processes (all substances will be seen as tracers)"
    echo
    echo "-eco [<bloom.spe>]"
    echo "       use BLOOM, optionally using an alternative algea database for the default $D3D_HOME/share/delft3d/bloom.spe"
    echo
    echo "-*"
    echo "       any other options are passed through to Delwaq to process"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
configfile=
procfile=
userprocfile=
eco=
userspefile=none
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

configfile=$1
case $configfile in
    -h|--help)
    print_usage_info
    ;;
esac

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
    -p)
    userprocfile="$1"
    shift
    ;;
    -eco)
    eco=true
    if [[ $# -ge 1 ]]
        then
        userspefile="$1" ## using only -eco would result in using the default spe-file in $D3D_HOME/share/delft3d/
    else
        userspefile=none
    fi
    ;;
    *)
    switches="$switches $key" ## always copy all additional arguments to delwaq
    ;;
esac
done

if [ ! -f $configfile ]; then
    if [ ! -f $configfile.inp ]; then
        echo "ERROR: configfile $configfile does not exist"
        print_usage_info
    fi
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

if [ ! "$userprocfile" == "" ]
    then
    procfile=$userprocfile
else
    procfile=$D3D_HOME/share/delft3d/proc_def
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

echo "    Configfile       : $configfile"
echo "    Procfile         : $procfile"
if [ "$eco" == "true" ]
   then
   echo "    bloom.spe file   : $spefile"
fi
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
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

if [ "$eco" == "true" ]
   then
        echo "executing:"
        echo "$bindir/delwaq $configfile -p $procfile -eco $spefile $switches"
        echo
        $bindir/delwaq1 $configfile -p "$procfile" -eco "$spefile" $switches
    else
        echo "executing:"
        echo "$bindir/delwaq $configfile -p $procfile $switches"
        echo
        $bindir/delwaq $configfile -p "$procfile" $switches
    fi

    #
    # Wait for any key to run delwaq 2
    #
wait
if [ $? == 0 ]
  then
    echo ""
    echo "Delwaq did run without errors."
else
    echo ""
    echo "Delwaq1 did not run correctly, ending calculation"
fi
    # Wait until all child processes are finished
wait
