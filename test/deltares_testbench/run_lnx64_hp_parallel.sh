#!/bin/bash

mpidir=/opt/mpich2/1.4.1_intel14.0.3_+mpd/bin

echo Starting mpd...
$mpidir/mpd &

echo Running mpdboot...
$mpidir/mpdboot -n 2

echo Starting testbench...
$1 $2 $3 $4 $5 $6 $7 $8 $9

if [ $? -ne 0 ]; then
    echo "ERROR running testbench"
    exit 1
fi

echo Running mpdexit...
$mpidir/mpdallexit

