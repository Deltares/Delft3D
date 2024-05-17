#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs D-Emissions on Linux
    #
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
D3D_HOME=$scriptdir/..
bindir=$D3D_HOME/bin
procfile=$D3D_HOME/share/d-emissions/em_proc_def

$bindir/run_delwaq.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -p "$procfile"
