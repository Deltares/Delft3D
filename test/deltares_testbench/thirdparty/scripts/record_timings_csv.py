# record_timings_csv.py -- (copied from record_timings.py)
#     Record the timings in the history file:
#     - Add the date to the history file
#     - Add the timings per case
#
#     For simplicity, put all items on the same line, separated by tabs
#
import os, sys
import re
from datetime import date


def getDFMVersion(dimrlog):

    version = "?"

    pattern = ">> dflowfm"
    prog = re.compile(pattern)

    filelog = open(dimrlog)

    for line in filelog:
#        print(line)
        if prog.search(line):
            words = line.split()
            version = words[-1]
            break

    return version


#
# Open the history file in append mode ...
#
if not os.path.exists('history'):
    os.makedirs('history')
if not os.path.isfile("history/timing_history.csv"):
    filehist = open("history/timing_history.csv", "w")
    filehist.write( "Date,version,computation time,model\n" )
else:
    filehist = open("history/timing_history.csv", "a")

currentDate = date.today().isoformat()

#
# Loop over the case directories
#
rootdir = "data/cases"

count = [0, 5, 20]

print(os.getcwd())

version = "?"

for subdir, dirs, files in os.walk(rootdir):

    if not os.path.exists(os.path.join(subdir,"timing.out")):
        continue


    if len(sys.argv) > 1:
        version = sys.argv[1]
    else:
        #
        # Get the version of D-Flow FM
        #
        if os.path.exists(os.path.join(subdir,"dimr_screen.log")):
            version = getDFMVersion(os.path.join(subdir,"dimr_screen.log"))

    #
    # Add the timings in "timing.out" to the history file
    #
    filetimes = open(os.path.join(subdir,"timing.out"))
    print("Processing timing.out for model: " + os.path.basename(subdir))

    i = 0
    for line in filetimes:
        words = line.split()
        timing = words[-1]
        ftiming = float(timing)
        timing = int(round(ftiming))

        filehist.write( currentDate + "," + version + "," + str(timing) + "," + os.path.basename(subdir) + "\n" )

        i = i + 1

    filetimes.close()

filehist.close()

