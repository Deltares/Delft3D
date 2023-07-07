#!/usr/bin/env python
import subprocess
import pprint
import argparse
import sys
import re
import shutil

###########################################################################
class svnclient:
    """ SVN client (in the absence of pysvn, just using the executable in the path """

    svnauthargs = "--username dscbuildserver --password Bu1lds3rv3r"

    def __init__(self):
        pass      # placeholder for custom constructor  

    def list(self,url):
        outp = subprocess.Popen("svn list "+self.svnauthargs+" "+url, shell=True, stdout=subprocess.PIPE).stdout.read().split("\n")
        outp = [ss for ss in outp if ss]     # remove empty strings 
        return (outp)

    def checkout(self,url,dir,depth):              
        print "svn checkout "+self.svnauthargs+" --depth "+depth+" "+url+" "+dir
        outp = subprocess.Popen("svn checkout "+self.svnauthargs+" --depth "+depth+" "+url+" "+dir, 
             shell=True, stdout=subprocess.PIPE).stdout.read()
        # Do somethin with the output
        return(0)

    def setdepth(self,dir,depth):              
        outp = subprocess.Popen("svn update "+self.svnauthargs+" --set-depth "+depth+" "+dir,
             shell=True, stdout=subprocess.PIPE).stdout.read()
        revision = -1 
        match=re.search('Updated to revision (\d*)', outp)
        if (match):
           revision=int(match.group(1))
        return(revision)

        # Do something with the output
###########################################################################


def parse_args():
    """
    Parse the command line arguments
    """
    argumentparser = argparse.ArgumentParser(
        description='Package testcases checked out from the testbench repos',
        prog='pkgtest', usage='%(prog)s [options]')
    argumentparser.add_argument('-u', '--url',
                   dest='baseURL',
                default='https://repos.deltares.nl/repos/DSCTestbench/trunk/cases/e02_dflowfm',
                   help='Base URL of repository')
    argumentparser.add_argument('-d', '--dir',
                   dest='baseDIR',
                default='selection',
                   help='Base destination directory of checkout')
    argumentparser.add_argument('-v', '--verbose',
                 action='store_true',
                   dest='verbose',
                default=False)
    argumentparser.add_argument('-l', '--list',
                 action='store_true',
                   dest='jadolist',
                default=False,
                   help='List testcases in the repository')
    argumentparser.add_argument('-f', '--file',
                   dest='listfile',
                default='',
                   help='File containing the list of selected cases')
    argumentparser.add_argument('-log', '--logfile',
                   dest='logfile',
                default='',
                   help='Dummy file to write debug info into')
    arguments = argumentparser.parse_args()
    pprint.pprint(arguments)
    return arguments

def list_repos(baseURL,svnclient):
    dirlist = svnclient.list(baseURL)

    for j in range(len(dirlist)):
        subdirlist = svnclient.list(baseURL+'/'+dirlist[j])
        for i in range(len(subdirlist)):
            testcasepath = subdirlist[i]
            names=testcasepath.split("/")
            print dirlist[j]+testcasepath
    return (dirlist)

# -------------------------------------------------------------------------------------------
# To get a list of testcase paths from the testbench configuration XML, I did something like:
# perl -n -e 'if(m/<path>e02_dflowfm\/(.*)<\/path>/) {print $1."\n"}' >list_testbench_tests.txt 

client = svnclient()
args = parse_args()
baseURL = args.baseURL
baseDIR = args.baseDIR
jadolist = args.jadolist
verbose = args.verbose
if (args.logfile>''):
   logfile = args.logfile
   jadolog = True
else:
   jadolog = False

if (jadolist):
   list_repos(baseURL,client)
   sys.exit()

if (args.listfile==''):
   sys.stderr.write("pkgtest - checks out a selected subset of test from the testbench\n")
   sys.stderr.write("          type pkgtest --help\n")
   sys.stderr.write("\n")
   sys.exit()

flist = open(args.listfile,'r')
selection = flist.readlines()

#Input in a tree
tree={}
for entry in selection:
    if (entry[0]!='#'):
        splitlist = ((entry.replace("\n","")).replace("\r","")).split('/')
        section = splitlist[0]
        testcase = splitlist[1]
        if not(section in tree):
            tree[section]=[]
        tree[section].append(testcase) 

if (jadolog):
    try:
        log = open(logfile,"w")
    except:
        sys.stderr.write("Could not open '"+logfile+"' for writing!\n")
        jadolog=False
             
# Make root
try:
    shutil.rmtree(baseDIR)
except:
    pass
revision = client.checkout(baseURL,baseDIR,depth='empty')                           # checkout root empty 

# Run the tree
for section,testcases in tree.iteritems():
    revision = client.setdepth(baseDIR+'/'+section, depth='empty')                  # update section empty
    if (verbose):
        sys.stderr.write("%8d %s\n" % (revision,section))   
    for testcase in testcases:
        revision = -1
        revision = client.setdepth(baseDIR+'/'+section+'/'+testcase, depth='infinity') # update modeldir full 
        if (jadolog):
            casefound=(revision>0)
            if casefound:
                log.write("##teamcity[message text='CHECK OUT' errorDetails='%s, revision %d.' status='NORMAL']\n" % (section+'/'+testcase,revision))
            else:
                log.write("##teamcity[message text='NOT FOUND' errorDetails='%s not found in section %s!' status='WARNING']\n" % (testcase,section))
                sys.stdout.write("##teamcity[message text='NOT FOUND        %s']\n" % (section+'/'+testcase))
        
try:
    shutil.rmtree(baseDIR+"/.svn")
except:
    pass

if jadolog:
    if (not(log.closed)):    
        log.close()

sys.exit()


