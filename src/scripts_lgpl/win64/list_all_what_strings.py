import argparse
import sys
import os
import string
from datetime import datetime


def strings(filename, min=4):
    #  with open(filename, errors="ignore") as f:  # Python 3.x
    with open(filename, "rb") as f:           # Python 2.x
        result = ""
        for c in f.read():
            if c in string.printable:
                result += c
                continue
            if len(result) >= min:
                yield result
            result = ""
        if len(result) >= min:  # catch result at EOF
            yield result


def list_what_strings(fname):
    if fname.endswith(".dll") or fname.endswith(".exe"):
        print("\t%s" % fname)
        sl = list(strings(fname))
        for s in sl:
            if s.find('@(#)') != -1:
                print("\t\t%s" % s[4:])


def recursive_walk(folder):
    for dirName, subdirList, fileList in os.walk(folder):
        print('%s' % dirName)
        #for sub_dir in subdirList:
        #    dir_name = os.path.join(dirName, sub_dir)
        #    print dir_name
        for fname in fileList:
            fname = os.path.join(dirName, fname)
            #print('\t\t%s' % fname)
            list_what_strings(fname)


def main(argv):
    parser = argparse.ArgumentParser(description='Batch process to remove side toc in HTML-files')
    # run_mode_group = parser.add_mutually_exclusive_group(required=False)
    parser.add_argument('-s', '--srcdir',
                        help="Directory on which the html-files are processed to remove 'sidetoc'.",
                        dest='src_dir')
    args = parser.parse_args()

    src_dir = '.'
    if args.src_dir:
        src_dir = args.src_dir
    if not os.path.exists(src_dir):
        print ("Given directory does not exists: %s" % src_dir)
        return

    start_dir = os.getcwd()
    os.chdir(src_dir)

    print("Root Directory: %s" % start_dir)
    recursive_walk(src_dir)
    print("Processing done")

    cwd = os.chdir(start_dir)
    return

if __name__ == "__main__":
    start_time = datetime.now()
    print 'Start: %s\n' % start_time

    filename = "dimr_version.lst"
    if os.path.exists(filename):
        os.remove(filename)
    print('Listing is written to: %s' % filename)
    sys.stdout = open(filename, "a")

    main(sys.argv[0:])

    sys.stdout = sys.__stdout__

    print '\nStart: %s' % start_time
    print 'End  : %s' % datetime.now()
    print 'Klaar'
