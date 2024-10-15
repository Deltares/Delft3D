'''
Author: Edwin Spee
E-Mail: edwin.spee@deltares.nl
Date  : 11 july 2019

This script counts the signed and unsigned files in the logfile from the 2.b DIMR_collector_release_signed
This is quite straight forward, except for the fact the logfile is utf-16 encoded.
'''

import io
import os.path

logfile = "sigcheck.log"
summary  = "summary.txt"

if (os.path.isfile(logfile)):

    f = io.open(logfile, 'r', encoding='utf-16-le')
    content = f.readlines()
    f.close

    unsigned = 0
    signed = 0

    for line in content:
        if line.find("Verified:") > -1:
            if line.find("Unsigned") > -1:
                unsigned += 1
            elif line.find("Signed") > -1:
                signed += 1

    total  = unsigned + signed

    f = open(summary, 'w')
    out = "Number of UNsigned files (Total number of files): " + str(unsigned) + " (" + str(total) + ")"
    f.write(out)
    f.close

else:
    print("ERROR: logfile (", logfile, ") not found.")

