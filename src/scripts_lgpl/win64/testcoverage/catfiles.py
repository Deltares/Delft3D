import os
import sys
import re
path=sys.argv[1]
pattern=sys.argv[2]
outname=sys.argv[3]
fout = open(outname,"w")
for (path, dirs, files) in os.walk(path):
    for file in files:
        if re.match(pattern,file):
            sys.stderr.write("%s\n"%os.path.join(path,file))
            try:
                fin = open(os.path.join(path,file),"r")
            except:
                sys.stderr.write("Could not open and read %s!\n"%os.path.join(path,file))
            for lin in fin.readlines():
                fout.write(lin)
            fin.close()
fout.close()


