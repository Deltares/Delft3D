#!/bin/bash
    #
    # Set the directory containing checkhydbal, and the required libraries
    #
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
exedir=$scriptdir
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH

    #
    # Run checkhydbal
    #
echo "Arguments: $1 $2 $3 $4 $5 ..."
echo "Starting checkhydbal in"
pwd
echo ""

rfil=`echo $1 | cut -d'"' -f 2`

echo Running: $exedir/checkhydbal $rfil $2 $3 $4 $5 $6
$exedir/checkhydbal $rfil $2 $3 $4 $5 $6

if [ $? == 0 ]
  then
    echo ""
    echo "checkhydbal did run without errors."
else
    echo ""
    echo "checkhydbal did not run correctly."
fi
