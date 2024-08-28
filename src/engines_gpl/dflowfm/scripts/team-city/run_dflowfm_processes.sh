#!/bin/bash

#
# Note: this script is deprecated. It calls run_dflowfm.sh instead
#

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`

$scriptdir/run_dflowfm.sh $*
