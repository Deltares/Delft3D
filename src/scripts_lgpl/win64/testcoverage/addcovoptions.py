from lxml import etree, objectify
import re
import sys
import os.path
import pprint as pp

def add_cov(inproj, outproj, CoveragePath):
  # coverage_options = " /Qcov-gen /Qcov-dir  "+ CoveragePath   # with path specification for output
    coverage_options = " /Qcov-gen "                            # without path specification for output
    xmltree = etree.parse(inproj)
    root=xmltree.getroot()
    for child in root:
        if (child.tag=='Configurations'):
            configs = child
            for child in configs:
                if (child.tag=='Configuration'):
                    config = child
                    for child in config:
                        if (child.tag=='Tool' and child.attrib['Name']=='VFFortranCompilerTool'):
                            addopts = ""
                            if 'AdditionalOptions' in child.attrib:
                                addopts = child.attrib['AdditionalOptions']
                            addopts = addopts + " " +coverage_options
                            child.attrib['AdditionalOptions'] = addopts
    xmltree.write(outproj, pretty_print=True, xml_declaration=True,   encoding="utf-8")
    return xmltree

def iter_projects(solution,covpath):
    fsln = open(solution,"r")
    slntxt = fsln.readlines()
    fsln.close()
    slndir = os.path.join(os.getcwd(),os.path.dirname(solution))
    for line in slntxt:
        m = re.search("Project\(.*\)\s*=\s*\"(.+)\"\s*,\s*\"(.+)\"\s*,\s*\"(.+)\"",line)
        if m:
            project_name = m.group(1)
            project_path = m.group(2)	# always relative wrt solution dir
            project_guid = m.group(3)
            project_full_path = os.path.join(slndir,project_path) 
            if re.match(".*proj$",project_full_path):
                inproj = project_full_path
                outproj = project_full_path
#               resultpath = os.path.join(slndir,covpath)
                resultpath = "."
                if not(os.path.exists(resultpath)):
                    os.makedirs(resultpath)
                add_cov(inproj, outproj, resultpath)
                # print (inproj + ' ---> ' + outproj)


fsln = sys.argv[1]
slndir = os.path.dirname(fsln)
iter_projects(fsln,'d:\tmp\cov')

