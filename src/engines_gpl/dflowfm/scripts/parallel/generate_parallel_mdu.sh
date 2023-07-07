#!/bin/bash
#
# Creates #np slightly changed copies of an original MDU to use for a parallel run.
# Note: The network files *_00xx.nc should have been partitioned before already.

function print_usage_info {
    echo "Usage: $(basename $0) <MDU-file> <number of processes> [partitioning polygon file] <Icgsolver>"
	echo "Polygon is optional and by default not used anymore"
    exit 1
}

## Start of actual script ##

# settings
NUMLENGTH=4

#read command line arguments
if [ $# -lt 3 ]; then
    echo "Error: Missing arguments."
    print_usage_info
    exit 1
fi

MDU_BASEFILE="$1"
NUMRANKS="$2"
if [ $# -gt 3 ]; then
	echo "Polygon detected"
	PART_FILE="$3"
	ICGSOLVER="$4"
elif [ $# -eq 3 ]; then
	ICGSOLVER="$3"
fi

#get basename
MD_IDENT=${MDU_BASEFILE%.mdu}

echo Scattering original MDU file \'$MDU_BASEFILE\' across partitioned files...
#scatter mdu-file
#  generate per-domain mdu-files

if [ ! -r $MDU_BASEFILE ]; then
    echo ERROR: can not read MDU-file \'$MDU_BASEFILE\'. Exiting.
    exit 1
fi

if [ -n $PART_FILE ] && [ ! -r $PART_FILE ] ; then
    echo "WARNING: can not read partitioning polygon file \'$PART_FILE\'."
    echo "         Make sure to add it to your input dir. Continuing anyway."
fi

if [ ! $NUMRANKS -ge 1 ]; then
    echo ERROR: invalid number or processes: \'$NUMRANKS\'. Exiting.
    exit 1
fi

# Also check whether MDU_BASEFILE contains all the required keys that are to be replaced.
error=0
if [ -n $PART_FILE ]; then
 for reqkey in NetFile PartitionFile Icgsolver; do
     if grep -q -i -e '^[ \t]*'$reqkey'[ \t]*=' $MDU_BASEFILE; then
         :
     else
         error=1
         echo ERROR: Required key \'$reqkey\' is missing in MDU-file \'$MDU_BASEFILE\'.
     fi
 done
else
 for reqkey in NetFile Icgsolver; do
     if grep -q -i -e '^[ \t]*'$reqkey'[ \t]*=' $MDU_BASEFILE; then
         :
     else
         error=1
         echo ERROR: Required key \'$reqkey\' is missing in MDU-file \'$MDU_BASEFILE\'.
     fi
 done
fi

if [ $error -ne 0 ]; then
    echo ERROR: Will not produce partitioned files. Exiting.
    exit 1
fi

## All is well, now do the actual scattering. ##

for (( my_rank=0; my_rank<NUMRANKS; my_rank++ ))
do
# add rank number to filename
  numstr="${my_rank}"
#   add leading zeros to rank string
  for (( i=${#my_rank}; i<NUMLENGTH; i++ ))
		do
		  numstr=0"${numstr}"
		done
  MDU_FILE="${MD_IDENT}_${numstr}.mdu"
		
#	copy base mdu-file to domain specific mdu-file and:
#   -add domain number to netfile
#   -modify snapshotdir
# NOTE: in regexp below:
# First, try to handle <mdident>_YYYYMMDD_HHMMSS_rst.nc
# Then, try to handle <mdident>_rst.nc ONLY IF no YYYYMMDD_HHMMSS pattern was present.
if [ -n $PART_FILE ]; then 
     sed -r -e "s/(NetFile.*)=\s*([.\/A-Za-z0-9_\-]*)_net\.nc(.*)/\1= \2_${numstr}_net\.nc\3/;
  s/(SnapshotDir.*)=[^#]*(#.*)?/\1= snapshots_${numstr} \2/i;
  s|(PartitionFile.*)=[^#]*(#.*)?|\1= ${PART_FILE} \2|i;
  s/(RestartFile.*)=\s*([.\/A-Za-z0-9_\-]*)_([0-9]{8}_[0-9]{6})_rst\.nc(.*)/\1= \2_${numstr}_\3_rst\.nc\4/i;
  /RestartFile.*=.*[0-9]{8}_[0-9]{6}_rst\.nc/!{s/(RestartFile.*)=\s*([.\/A-Za-z0-9_\-]*)_rst\.nc(.*)/\1= \2_${numstr}_rst\.nc\3/i}
  s/(Icgsolver[ ]*)=[^#]*(#.*)?/\1= ${ICGSOLVER} \2/i" $MDU_BASEFILE > $MDU_FILE
  echo " #$my_rank: $MDU_FILE"
else
    sed -r -e "s/(NetFile.*)=\s*([.\/A-Za-z0-9_\-]*)_net\.nc(.*)/\1= \2_${numstr}_net\.nc\3/;
  s/(SnapshotDir.*)=[^#]*(#.*)?/\1= snapshots_${numstr} \2/i;
  s/(RestartFile.*)=\s*([.\/A-Za-z0-9_\-]*)_([0-9]{8}_[0-9]{6})_rst\.nc(.*)/\1= \2_${numstr}_\3_rst\.nc\4/i;
  /RestartFile.*=.*[0-9]{8}_[0-9]{6}_rst\.nc/!{s/(RestartFile.*)=\s*([.\/A-Za-z0-9_\-]*)_rst\.nc(.*)/\1= \2_${numstr}_rst\.nc\3/i}
  s/(Icgsolver[ ]*)=[^#]*(#.*)?/\1= ${ICGSOLVER} \2/i" $MDU_BASEFILE > $MDU_FILE
  echo " #$my_rank: $MDU_FILE"
fi
done

