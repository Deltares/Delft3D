#!/usr/bin/env python
import os, re
import pprint as pp
import glob
import shutil
import sys

def copy_cases(root,logname,platform):
    failed = {}
    for flogname in glob.glob(os.path.join(root,"logs",logname)):
        with open(os.path.join(flogname),"r") as flog:
            while True:
                line = flog.readline()
                if not line:
                    break
                if r"Executing (in separate thread)" in line:
                    m = re.search(r"::in directory::([^\s]+)",line)
                    if m:
                        testpath = m.group(1)
                lup = line.upper()		
                if (("COMPARISONRUNNER.POST_PROCESS" in lup and ("NOK" in lup or "ERROR" in lup)) or "EXCEPTION:" in lup or "[ERROR  ]" in lup):
                    failed[testpath] = "yes"
    
    shutil.rmtree("failed",ignore_errors=True)
    os.mkdir("failed")
    
    scriptdir = os.path.dirname(os.path.realpath(__file__))
    lijst = open(os.path.join('failed','list.txt'),"w")
    for src_case in failed.keys():
        # copy case directory
        dst_case = os.path.join('failed',src_case)
        lijst.write("%s => %s"%(src_case,dst_case))
        try:
            shutil.copytree(src_case,dst_case)
            lijst.write("  COPY OK\n")
        except:
            lijst.write("  COPY FAILED\n")

        # copy reference directory
        src_ref = src_case.replace("cases",os.path.join("references",platform))
        dst_ref = os.path.join('failed',src_ref)
        lijst.write("%s => %s"%(src_ref,dst_ref))
        try:
            shutil.copytree(src_ref,dst_ref)
            lijst.write("  COPY OK\n")
        except:
            lijst.write("  COPY FAILED\n")
    lijst.close()

def main():
    platform = sys.argv[1]
    copy_cases(".","testbench.log*",platform)


if __name__ == "__main__":
   main()
