
import os, re, sys, glob
import argparse
import filecmp

parser = argparse.ArgumentParser(description='Test Bench Version 3, test runner for black box tests.')

parser.add_argument("--ref",
                    help="Name of reference file",
                    default=None,
                    required=True,
                    dest="reffile")
parser.add_argument("--new",
                    help="Name of new file",
                    default=None,
                    required=True,
                    dest="newfile")
args = parser.parse_args()

with open(args.__dict__["newfile"], "r") as filinhandle:
    newFileContents = filinhandle.readlines()
filinhandle.closed

fout = open("new_noversion.log", "w")
for line in newFileContents:
	if line.startswith("Source:"):
		continue
	startposVersion = str(line).lower().find("version")
	if startposVersion>-1:
		line = line[:startposVersion+7] + "\n"
	fout.write(line)
fout.close()

print("##teamcity[testStarted name='dflowfm --version']")
if filecmp.cmp(args.__dict__["reffile"], "new_noversion.log"):
    print("##teamcity[message text='INFO - Test succeeded: file is equal to the reference file']")
else:
    print("##teamcity[testFailed name='dflowfm --version' message='ERROR file differs from reference file']")
print("##teamcity[testFinished name='dflowfm --version']")
