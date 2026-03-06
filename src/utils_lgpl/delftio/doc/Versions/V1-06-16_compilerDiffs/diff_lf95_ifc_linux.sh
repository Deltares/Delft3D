#!/bin/sh
#
# diff_lf95_ifc_linux.sh: diff all testresults, ignoring spaces
#                        (useful for compiler version changes).
#
# Stef.Hummel@deltares.nl
#
# (c) Deltares, 2026
#

diffResult="diff_lf95_ifc_linux-res.txt"
diffBinaries="diff_lf95_ifc_linux-binaries.txt"

echo "NON SPACE DIFFERENCES IN:" > $diffResult

for d in resultsApproved/*
do
	if [ -d $d ] ; then
		dirName=`basename $d`
		echo "diffing in dir. $dirName"
		for f in $d/intel/*
		do
			if [ -f $f ] ; then
				fileName=`basename $f`
				orgName=resultsApproved/$dirName/w32/$fileName
				diffName=progsrc/$dirName/unix/$fileName
				echo "diffing -q $orgName $diffName"
				diff -q $orgName $diffName
				if [ $? != 0 ] ; then
					echo "DIFFING -w $orgName $diffName"
					diff -w $orgName $diffName
				fi
			fi
		done
	fi
done > $diffResult 2>&1

grep '^Binary files' $diffResult > $diffBinaries 2>&1

