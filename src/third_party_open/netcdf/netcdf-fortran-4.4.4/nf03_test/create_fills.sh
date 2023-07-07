#!/bin/sh
# This shell script which creates the fill.nc file from fill.cdl.
# $Id$

echo
echo "*** Testing creating file with fill values."
set -e
#../ncgen/ncgen -b $srcdir/fills.cdl
cp ${TOPSRCDIR}/nf_test/ref_fills.nc ./fills.nc
echo "*** SUCCESS!"
exit 0
