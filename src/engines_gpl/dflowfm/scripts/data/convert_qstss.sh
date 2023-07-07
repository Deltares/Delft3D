#!/bin/bash
# $Id$
## DESCRIPTION:
## Converts the .tim files from discharge_salinity_temperature_sorsin
## providers (i.e., with 4 columns) into valid new .tim files with only
## the relevant columns.
## It cuts out the salinity and/or temperature column, based on the
## specified command line arguments.
##
## Usage:
##    convert_qstss.sh <input.tim|input.ext> <jasal> <jatem>
##
## When input file is an .ext file, all contained qstss .tim files are
## automatically detected.

me=`basename "$0"`

function print_usage {
	echo <<EOM
Usage:
    $me <input.tim> <jasal> <jatem>

jasal and jatemp should be 0 or 1.
EOM
}

function extract_qstss_tim_from_ext {
	timfiles=`grep -A 1 --color=no -e '^QUANTITY\s*=\s*discharge_salinity_temperature_sorsin' $1 | \
			  grep --color=no -e '^FILENAME' | \
			  sed -e 's/^FILENAME[ \t]*=[ \t]*\([^ \t][^ \t]*\)\.pli.*/\1.tim/'`
}

if [ $# -ne 3 ]; then
	echo "$me: Error: specify exactly three arguments."
	print_usage
	exit 1
fi

## START
echo "Input file:       $1"
echo "Keep salinity:    $2"
echo "Keep temperature: $3"

## COLUMN SELECTION
colselect='\1\t\2'
if [ "$2" -eq 1 ]; then
	colselect="$colselect\\t\\3"
fi
if [ "$3" -eq 1 ]; then
	colselect="$colselect\\t\\4"
fi

#DEBUG
#echo Colselect: $colselect

## TIM FILES SELECTION
if [[ "$1" =~ .*\.ext ]]; then
	echo
	echo Detected input "$1" is an .ext file. Will scan for discharge_salinity_temperature_sorsin providers...
	extract_qstss_tim_from_ext "$1"
	echo Found these tim files: $timfiles
else
#DEBUG
#	echo Assuming input "$1" is a .tim file
	timfiles="$1"
fi


## PROCESS THE TIM FILES

#DEBUG
#rm -f cpcmds.txt

echo
for fn in $timfiles; do
    if [ ! -f "$fn" ]; then
    	echo "WARNING: file \"$fn\" not found! Skipping and continuing."
    	continue
    fi

    echo -n "Processing \"$fn\"... (Backup in: $fn.bak)"
    sed -i.bak -e 's/^\s*\([0-9EDed.+-]\{1,\}\)\s\{1\}\([0-9EDed.+-]\{1,\}\)\s\{1,\}\([0-9EDed.+-]\{1,\}\)\s\{1,\}\([0-9EDed.+-]\{1,\}\).*/'$colselect'/' $fn
    if [ "$?" -eq 0 ]; then
    	echo -e "\t[OK   ]"
    else
    	echo -e "\t[ERROR]"
    fi

	#DEBUG
	#echo cp $fn.bak $fn >> cpcmds.txt
done

#DEBUG
#echo
#cat cpcmds.txt
