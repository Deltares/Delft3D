# chk_timings.py --
#     Check that the timings have not deteriorated:
#     - Values in the log file that are lower than in the reference file are "always" okay
#     - Values in the log file that are 5% larger are okay
#     - Values in the log file that are more than 5% larger are NOT okay
#     The result is the maximum value that was amiss or zero, to allow the regular testbench
#     stuff to do its job.
#

import os

#
# Construct the path to the reference file
#
curdir = os.getcwd()
refdir = curdir.replace( "cases", "references/lnx64" )

#
# Open the files
#
fileref = open(refdir + "/timing.out")
filenew = open("timing.out")
filerpt = open("timing.result", "w")

#
# Determine whether the performance test should be considered successful
#
result = 1.0
while 1:
    lineref = fileref.readline()
    linenew = filenew.readline()

    if lineref == "":
        break

    words    = lineref.split()
    reftimer = float(words[-1])

    words    = linenew.split()
    newtimer = float(words[-1])

    ratio = newtimer / reftimer
    if ratio > 1.05 and ratio > result:
        result = max( result, ratio )

filerpt.write( "Maximum ratio: " + str(result) + "\n" )
filerpt.close()
