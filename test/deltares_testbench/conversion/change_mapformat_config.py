import os
import argparse


def replace_mapformat(configfile, chapter):
    f = open(configfile, 'r')
    content = f.readlines()
    f.close

    old = ["s0", "s1", "unorm", "Patm"]
    new = ["mesh2d_s0", "mesh2d_s1", "mesh2d_ucmag", "mesh2d_Patm"]

    isChanged = False
    inChapter = False
    newcontent = []
    for line in content:
        line2 = line
        if line2.find("<testCase name") > -1:
            inChapter = line2.find(chapter) > -1
        else:
            if inChapter and (line2.find("parameter name") > -1) and (line2.find("mesh2d") < 0):
                for i in range(2):
                    if line2.find(old[i]) > -1:
                        line2 = line2.replace(old[i], new[i])
                        isChanged = True

        newcontent.append(line2)

    if isChanged:
        f = open(configfile, 'w')
        f.writelines(newcontent)
        f.close

parser = argparse.ArgumentParser(description='Update config file as output format changes to ugrid output.')
parser.add_argument('configFile', type=str, help='filename of the configuration file')
parser.add_argument('chapter', type=str, help='part of the configuration file to update')

args = parser.parse_args()

replace_mapformat(args.configFile, args.chapter)

