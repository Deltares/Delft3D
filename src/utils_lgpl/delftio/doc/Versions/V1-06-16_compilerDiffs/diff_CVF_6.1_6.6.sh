#!/bin/sh
#
# diff_CVF_6.1_6.6.sh: diff all testresults, ignoring spaces
#                      (useful for compiler version changes).
#
# Stef.Hummel@deltares.nl
#
# (c) Deltares, 2026
#

diffResult="diff_CVF_6.1_6.6-res.txt"
diffBinaries="diff_CVF_6.1_6.6-binaries.txt"

echo "NON SPACE DIFFERENCES IN:" > $diffResult

for d in resultsApproved/*
do
	if [ -d $d ] ; then
		dirName=`basename $d`
		echo "diffing in dir. $dirName"
		for f in $d/w32/*
		do
			if [ -f $f ] ; then
				fileName=`basename $f`
				diffName=progsrc/$dirName/w32/$fileName
				echo "diffing -q $f $diffName"
				diff -q $f $diffName
				if [ $? != 0 ] ; then
					echo "DIFFING -w $f $diffName"
					diff -w $f $diffName
				fi
			fi
		done
	fi
done > $diffResult 2>&1

grep '^Binary files' $diffResult > $diffBinaries 2>&1

