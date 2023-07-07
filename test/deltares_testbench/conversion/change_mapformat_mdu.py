import os, os.path

def replace_mapformat(mdufile):
    f = open(mdufile, 'r')
    content = f.readlines()
    f.close

    isChanged = False
    newcontent = []
    for line in content:
        line2 = line.strip('\n')
        if line2.find("MapFormat") > -1:
            isChanged = line2.find("= 1") > -1
            line2=line2.replace("= 1", "= 4")
            if line2.find("#") > -1:
                if line2.find("NetCDF-UGRID") < 0:
                    isChanged = True
                    if line2.find(")") > -1:
                       line2 = line2.replace(")", ", 4: NetCDF-UGRID)")
                    else:
                       line2 += ", 4: NetCDF-UGRID"
        line2 += '\n'
        newcontent.append(line2)

    if isChanged:
        f = open(mdufile, 'w')
        f.writelines(newcontent)
        f.close

directory = '.'

for root, dirs, files in os.walk(directory):
    for f in files:
        fullpath = os.path.join(root, f)
        if os.path.splitext(fullpath)[1] == '.mdu':
             replace_mapformat(fullpath)

