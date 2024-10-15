import argparse
import sys
import os
import string
from datetime import datetime

'''
Author: Jan Mooiman
E-Mail: jan.mooiman@deltares.nl
Date  : 10 sep 2017

This script list all what strings, starting with @(#)Deltares, in all subdirectories of a given root directory
The root directory is specified by the argument --srcdir .....

'''

global log_file


def lprint(*args, **kwargs):
    global log_file
    log_file.write(' '.join(map(str, args)) + '\n')


def strings(filename, min=4):
    if sys.version_info.major == 2:
        f = open(filename, "rb")  # Python 2.x
    else:
        f = open(filename, "r", errors='ignore') # Python 3.x
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
    f.close()


def list_what_strings(fname):
    print("\t\t%s" % fname)
    sl = list(strings(fname))
    what_string = []
    for s in sl:
        if s.find('@(#)Deltares') != -1:
            what_string.append(s[s.find('@(#)Deltares'):])
        if s.find('HeadURL') != -1:
            what_string.append(s[s.find('HeadURL'):])
    if what_string.__len__() != 0:
        lprint("\t%s" % fname)
        for s in what_string:
            if s[0:4] == '@(#)':
                lprint("\t\t%s" % s[4:])
            if s[0:7] == 'HeadURL':
                lprint("\t\t%s" % s[9:])


def recursive_walk(folder):
    for dirName, subdirList, fileList in os.walk(folder):
        if dirName != folder:
            print("\t%s" % dirName)
            for fname in fileList:
                fname = os.path.join(dirName, fname)
                if fname.find('.svn') == -1:
                    list_what_strings(fname)


def main(start_dir, src_dir):
    os.chdir(src_dir)

    print("Root Directory: %s" % src_dir)
    lprint("Root Directory: %s" % src_dir)
    recursive_walk(src_dir)
    print("Processing done")
    lprint("Processing done")

    cwd = os.chdir(start_dir)
    return


if __name__ == "__main__":
    global log_file

    start_time = datetime.now()

    parser = argparse.ArgumentParser(description='Batch process to list all what-strings')
    # run_mode_group = parser.add_mutually_exclusive_group(required=False)
    parser.add_argument('-s', '--srcdir',
                        help="Root directory from the what-strings are listed",
                        dest='src_dir')
    parser.add_argument('-o', '--output',
                        help="Output filename.",
                        dest='out_put')
    args = parser.parse_args()

    src_dir = '.'
    out_put = 'dimr_version.txt'
    if args.src_dir:
        src_dir = args.src_dir
    start_dir = os.getcwd()
    src_dir = os.path.normpath(os.path.join(start_dir, src_dir))
    if not os.path.exists(src_dir):
        print ("Given directory does not exists: %s" % src_dir)

    if args.out_put:
        out_put = args.out_put
    if os.path.exists(out_put):
        os.remove(out_put)
    log_file = open(out_put, "a")

    print('Start: %s\n' % start_time)
    lprint('Start: %s\n' % start_time)
    print("%s\n" % sys.version)

    print('Listing is written to: %s' % out_put)

    main(start_dir, src_dir)

    lprint('\nStart: %s' % start_time)
    lprint('End  : %s' % datetime.now())
    lprint('Klaar')
    print('\nStart: %s' % start_time)
    print('End  : %s' % datetime.now())
    print('Klaar')
