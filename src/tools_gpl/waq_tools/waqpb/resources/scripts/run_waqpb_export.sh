#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

## Set the directories containing the binaries
scriptdirname=`readlink \-f \$0`
bindir=`dirname $scriptdirname`
libdir=$bindir/../lib
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
echo
echo "    bin dir           : $bindir"
echo "    lib dir           : $libdir"

## Run
echo "executing:"
echo "$bindir/waqpb_export"
echo
$bindir/waqpb_export "$@"

## Wait until all child processes are finished
wait

