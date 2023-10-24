import os
import os.path
import re


def replace_keyword(inifile, oldkey, newkey):
    origfile = open(inifile, 'r')
    origlines = origfile.readlines()
    origfile.close()

    changed = False
    newcontent = []
    for origline in origlines:
        newline = origline.strip('\n')
        if re.search(r'\b' + oldkey + r'\b', newline):
            changed = True
            newline = newline.replace(oldkey, newkey)
        newline += '\n'
        newcontent.append(newline)

    if changed:
        newfile = open(inifile, 'w')
        newfile.writelines(newcontent)
        newfile.close()


directory = '.'

# read lists of old and new keywords from file
keyfile = open('keywords.txt', 'r')
lines = keyfile.readlines()
keyfile.close()

oldkeys = []
newkeys = []
old = True
for line in lines:
    line = line.strip()
    if line == 'OLD':
        continue
    if line == 'NEW':
        old = False
        continue
    if old:
        oldkeys.append(line)
    else:
        newkeys.append(line)

if len(oldkeys) != len(newkeys):
    raise ImportError("Check your input file, the number of OLD and NEW keys differ!")
d = dict(zip(oldkeys, newkeys))

for root, dirs, files in os.walk(directory):
    for f in files:
        fullpath = os.path.join(root, f)
        if os.path.splitext(fullpath)[1] == '.mdu':
            mdufile = open(fullpath, 'r')
            content = mdufile.readlines()
            mdufile.close()

            for line in content:
                line_stripped = line.strip('\n')
                if line_stripped.find("StructureFile") > -1:
                    [key, value] = line_stripped.split("=")
                    tmp = value.split("#")
                    structfile = tmp[0].strip(" ")

                    structfiles = []
                    if len(structfile) > 0:
                        # possibly split another time for the case of more than 1 filename on 1 line
                        for item in structfile.split(" "):
                            structfiles.append(item)

                        for file in structfiles:
                            print(root, file)
                            for key in d.keys():
                                replace_keyword(os.path.join(root, file), key, d[key])
