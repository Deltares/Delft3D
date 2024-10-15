import re
import os
import sys
import os.path
import subprocess

def runThisTest(dir, files):
    for fname in files:
        mcfg = re.match(r".*cfg$",fname)
        if (mcfg):
#           read the configuration file
            ftestconfig = open(os.path.join(dir,fname),"r")
            cfglines = ftestconfig.readlines()
            for cfgline in cfglines:
                mtest=re.search("\[(.*)\]",cfgline)
                if (mtest):
                    testname = mtest.groups()[0]

            ftestconfig.close()
#           launch the EC-testprogram in the test directory         
            ret = subprocess.call([abs_exec] + args + [fname], cwd=dir)
            if (ret!=0):
                sys.stdout.write("##teamcity[testFailed name='"+testname+"' message='Crash']\n")
                sys.stdout.write("##teamcity[testFinished name='"+testname+"' message='Comparison failed']\n\n")

print(os.path.dirname(os.path.realpath(__file__)))
executable = ""

# On TeamCity environment variable LD_LIBRARY_PATH or PATH is set before running this script.
# When running locally, you should set this for your local environment. 
# ~/build/lnx64/*/share/lib
# ~/build/x64/*/share/bin
# The location of the executable is probably different as well. 

if os.name == 'posix':
    executable = "ec_module_test"
if os.name == 'nt':
    executable = "ec_module_test.exe"

if (executable == ""):
    sys.stderr.write("Invalid OS = "+os.name)
    sys.exit()

if len(sys.argv)>1:
    executable = sys.argv[1]

abs_exec = os.path.abspath(executable)
args = ["-v", "-c"] 

for root, dirs, files in os.walk("."):
    runThisTest(root, files)
