#!/bin/bash
###
### This script converts the time series data for a single variable
### from a NetCDF his file into one time series file per station.
### Can be used for example to create noos-format files from a _his.nc
### $Id$
###
### Usage:
### nchis2noos.sh HIS_NC_FILE VARIABLENAME [STATIONNAME]
###

if [ $# -lt 2 ]; then
	echo "Error: Missing arguments."
	echo "Usage: $0 history_his.nc variablename [stationname]"
	exit 1
fi

if [ -r $1 ]; then
	hisfile=$1
else
	echo "Error: input history file could not be read."
	exit 1
fi

varname=$2

if [ -n "$3" ]; then
	statname=$3
fi


### Extract variable data from nc file
dumpfile=`mktemp`
ncdump -v $varname $hisfile > $dumpfile
if [ $? -ne 0 ]; then
	echo "Error: variable '$varname' not found in file '$hisfile'."
	exit 1;
fi


### Determine number of stations
numstat=`ncdump -h $hisfile | grep --color=no 'station ='|sed -r -e 's/\s*station =\s*([0-9]+).*/\1/'`
echo "Found $numstat stations in file $hisfile."

### Find index nr of requested station (if arg $3 was present)
if [ -n "$statname" ]; then
	statfile=`mktemp`
	ncdump -v station_name $hisfile > $statfile
	nline=`grep --color=no -n "station_name =" $statfile | cut -d":" -f1`
	nline=`expr $nline + 1`

	nstatnr=`grep --color=no -n "\"$statname\"" $statfile | cut -d":" -f1`
	if [ -z "$nstatnr" ]; then
		echo "Error: station '$statname' not found in file '$hisfile'."
		echo "The following stations are available:"
		tail -n +$nline $statfile | head -n -1
		exit 1
	fi

	# Now substract starting line of station variable from this station line, to get actual index nr of this station.
	nstatnr=`expr $nstatnr - $nline + 1`
fi


### Extract variable data from nc file (cut away header lines)
nline=`grep --color=no -n "$varname =" $dumpfile | cut -d":" -f1`
nline=`expr $nline + 1`
rawdatafile=`mktemp`
tail -n +$nline $dumpfile | head -n -1 | sed -e 's/\;//' > $rawdatafile


### Extract time data from nc file (cut away header lines)
timefile=`mktemp`
ncdump -l 100000000 -v time $hisfile > $dumpfile
if [ $? -ne 0 ]; then
	echo "Error: variable 'time' not found in file."
	exit 1;
fi

nline=`grep --color=no -n "data:" $dumpfile | cut -d":" -f1`
# NOTE: time values start directly on the same line, so different nline nr than for $varname above.
nline=`expr $nline + 2`

tail -n +$nline $dumpfile | head -n -1 | sed -r -e 's/time =//;s/,/\n/g;s/\;//' > $timefile


### Produce a separate file for each location (or the single one requested)
if [ -n "$nstatnr" ]; then
	nstart=$nstatnr
	nend=$nstatnr
else
	nstart=1
	nend=$numstat
fi

for (( i=$nstart; i <= $nend; i++ ))
do
	echo -n "Getting $varname values at location #$i from $hisfile... "
	outfile="stat${i}_$varname.tim"
	cut -d"," -f$i $rawdatafile | paste $timefile - > $outfile
	echo "Produced $outfile"
done


### Clean up
rm -f $dumpfile
rm -f $rawdatafile
rm -f $timefile

