# record_timings.py --
#     Record the timings in the history file:
#     - Add the date to the history file
#     - Add the timings per case
#
#     For simplicity, put all items on the same line, separated by tabs
#
import os
import re
from datetime import date


def getDFMVersion(dimrlog):

    version = "?"

    pattern = ">> dflowfm"
    prog = re.compile(pattern)

    filelog = open(dimrlog)

    for line in filelog:
        print(line)
        if prog.search(line):
            words = line.split()
            version = words[-1]
            break

    return version


#
# Open the history file in append mode ...
#
filehist = open("history/timing.history", "a")

currentDate = date.today().isoformat()

#
# Loop over the case directories
#
rootdir = "data/cases/e02_dflowfm/f031_performance_transport"

count = [0, 5, 20]

print(os.getcwd())

version = "?"

for subdir, dirs, files in os.walk(rootdir):

    if not os.path.exists(os.path.join(subdir,"timing.out")):
        continue

    #
    # Get the version of D-Flow FM
    #
    if os.path.exists(os.path.join(subdir,"dimr_screen.log")):
        version = getDFMVersion(os.path.join(subdir,"dimr_screen.log"))

    #
    # Add the timings in "timing.out" to the history file
    #
    filetimes = open(os.path.join(subdir,"timing.out"))

    i = 0
    for line in filetimes:
        words = line.split()
        timing = words[-1]

        filehist.write( currentDate + "\t" + version + "\t" + str(count[i]) + "\t" + timing + "\t" + os.path.basename(subdir) + "\n" )

        i = i + 1

    filetimes.close()

filehist.close()
