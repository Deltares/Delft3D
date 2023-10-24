#/usr/bin/python
#
# extractlines.py --
#     Extract lines from a text file that conform to a particular pattern
#
#     Arguments:
#     (1) infile    - File to read from
#     (2) outfile   - File to write the output to
#     (3) pattern   - Pattern to match the line to
#     (4) append    - Whether to append (1) to the output or not (0)
#
#     Note:
#     This is a very primitive version of "grep", but it needs to work
#     on both Windows and Linux
#
import sys
import re

#
# Simple extraction of lines matching a given pattern
#
def main(argv):
    infileName = sys.argv[1]
    outfileName = sys.argv[2]
    pattern = sys.argv[3]
    append = sys.argv[4]

    pattern = 'total computation time \(s\)'

    infile = open(infileName)

    if append == "1":
        outfile = open(outfileName, "a")
    else:
        outfile = open(outfileName, "w")

    prog = re.compile(pattern)

    for line in infile:
        match = prog.search(line)
        if match:
            outfile.write(line)

#
# Get it started
#
if __name__ == "__main__":
    main(sys.argv[:])
