import argparse
import subprocess
from pathlib import Path

# Get commandline arguments
parser = argparse.ArgumentParser(description='Directory synchronization and cleanup tool.')

parser.add_argument("--source",
                    default="",
                    required=True,
                    help="Path to source directory",
                    dest="sourcedir")
parser.add_argument("--target",
                    default="",
                    required=True,
                    help="Path to target directory",
                    dest="targetdir")

args = parser.parse_args()

# rootdir is the location of this script
rootdir = Path.cwd()

olddir = rootdir / Path(args.__dict__["targetdir"]).resolve()
newdir = rootdir / Path(args.__dict__["sourcedir"]).resolve()

if not olddir.is_dir():
   print("ERROR: target directory does not exist: " + str(olddir))
   exit(1)
if not newdir.is_dir():
   print("ERROR: source directory does not exist: " + str(newdir))
   exit(1)

for p in olddir.glob("**/*"):
    if not p.is_file(): continue
    lpfile = p.resolve()

    rel_p = lpfile.relative_to(olddir)
    lpfilenew = newdir / rel_p

    if not lpfilenew.is_file():
        cmd = "svn remove {}".format(lpfile)
        print(cmd)

        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
        (output, err) = process.communicate()
        print("  output:" + str(output))
        print("  err   :" + str(err))

print ("Finished")
