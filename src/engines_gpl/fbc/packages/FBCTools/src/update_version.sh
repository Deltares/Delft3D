#!/bin/bash

# Script to extract version information from SVN and version_number.ini

# Extract information
REVISION=`cd $1 && svn info | grep Revision | tr -d '\r\n' | sed -e 's/Revision: //'`
VN_MAJOR=`grep major $1/version_number.ini | tr -d '\r\n' | sed -e 's/major    = //'`
VN_MINOR=`grep minor $1/version_number.ini | tr -d '\r\n' | sed -e 's/minor    = //'`

# Compose version.h
cp $1/version.h.svn $1/version.h
sed -e "s/VN_MAJOR/$VN_MAJOR/" -i $1/version.h
sed -e "s/VN_MINOR/$VN_MINOR/" -i $1/version.h
sed -e "s/VN_BUILD_NUMBER/$REVISION/" -i $1/version.h