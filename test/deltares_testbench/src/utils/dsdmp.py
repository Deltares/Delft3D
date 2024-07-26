import re

import numpy as np
import Tkinter as tk


class DSTreeException(BaseException):
    def __init__(self, m) -> None:
        self.message = m

    def __str__(self) -> str:
        return self.message


# ------------------------------------------------------------- dictionary of substitutions for known errors
translations = {
    "PARTIAL FACTORS EC7 B": "PARTIAL FACTORS EC7B",
    "ECHO OF MSHEET INPUT": "INPUT DATA",
    "[END OF DUMP]": "",
}

# ------------------------------------------------------------- dictionary of substitutions for known errors
# * requires numpy to be imported as np

DEBUG = False


class dsdmp(object):
    def __init__(self) -> None:
        print("hello")


def split_table_row_into_floats(ss):
    s = re.split(r"[*!/#']*", ss)
    m = re.split(r"[\s\t;:,]*", s[0].lstrip().rstrip())
    a = []
    for mm in m:
        try:
            ff = float(mm)
        except:
            ff = -999
        a.append(ff)
    return np.array(a)


def trans(strin):
    strout = strin

    for key, value in str.translate.iteritems():
        strout = strout.replace(key, value)

    return strout


def dumptxt(hdr, txt, tktext, wintxt) -> None:
    if tktext is None:
        print("\n\n\n")
        print(f"{hdr}\n\n")
        for line in txt:
            print(f"    {line}")
    else:
        wintxt.wm_title(hdr)
        tktext.delete(1.0, tk.END)
        for line in txt:
            tktext.insert(tk.END, line + "\n")


def dumptree(branch, lvl) -> None:
    for key, vals in branch.iteritems():
        for val in vals:
            if type(val) == dict:
                print("   " * lvl + f"|--{key}")
                dumptree(val, lvl + 1)


def dumptreepaths(cls, branch, pad) -> None:
    for key, vals in branch.iteritems():
        for val in vals:
            if type(val) == dict:
                print(f"{pad}>{key}")
                dumptreepaths(val, f"{pad}>{key}")


class DSeriesFile:
    def __init__(self, sfdmp) -> None:
        self.filename = sfdmp
        dumpfile = open(self.filename, "r")
        self.tree_data = {}

        # Parse dumpfile
        try:
            line = [0]
            tree_result = DSeriesFile.__branch("", dumpfile, 0, line)
            self.tree_data[sfdmp] = [tree_result[0]]
        except DSTreeException as dse:
            raise dse
        dumpfile.close()

    def get_branch_from_path(self, path):
        br = self.tree_data
        elements = path.split(">")
        foundpath = ""
        for element in elements:
            if element > "":
                if ":" in element:
                    element, ndx = element.split(":")
                else:
                    ndx = 0
                if element in br:
                    if int(ndx) > 0:
                        foundpath = f"{foundpath}>{element}[{ndx}]"
                    else:
                        foundpath = f"{foundpath}>{element}"
                    br = br[element][int(ndx)]
        return [foundpath, br]

    def get_table(self, path):
        foundpath, br = self.get_branch_from_path(path)
        if ("COLUMN INDICATION" in br) and ("COLUMN DATA" in br):
            tbldata = dict()
            colnames = []
            for colname in br["COLUMN INDICATION"][0]["txt"]:  # read headers
                colnames.append(colname)
                tbldata[colname] = np.array([])
            ncol = len(colnames)
            for row in br["COLUMN DATA"][0]["txt"]:  # read data rows
                fields = split_table_row_into_floats(row)
                if len(fields) >= ncol:
                    for icol in range(ncol):
                        tbldata[colnames[icol]] = np.append(tbldata[colnames[icol]], fields[icol])
                else:
                    print(f"Expecting at least {ncol} fields, but found {len(fields)} ... skipping line ...")
                    print(f"Line says ...: '{row}'")
            return [foundpath, tbldata]
        else:
            return [foundpath, None]

    def get_txt(self, path):
        foundpath, br = self.get_branch_from_path(path)
        if "txt" in br:
            dmp_comparer.compareTxt([foundpath, br["txt"]], [foundpath, br["txt"]])
            return [foundpath, br["txt"]]
        else:
            # dmp_comparer.compareTxt([foundpath, None],[foundpath, None])
            return [foundpath, None]

    def list(self) -> None:
        dumptree(self.tree_data, 0)

    @classmethod
    def __branch(cls, branchname, dmp, lvl, line):
        global DEBUG
        newbranch = {}
        opened = line[0]
        if DEBUG:
            print(f"{line[0]:8d}   " * lvl + f"OPENING {branchname}")
        while True:
            dmpline = dmp.readline()
            dmpline = trans(dmpline)
            line[0] = line[0] + 1
            if dmpline:
                m = re.search(r"^\s*\[\s*(.*)\]\s*$", dmpline)
                if m:
                    tag = m.group(1).upper().rstrip().lstrip()
                    #                   tag = trans(tag);
                    m_end = re.search(r"END OF (.*)", tag)
                    m_nxt = re.search(r"NEXT OF (.*)", tag)
                    if m_end:
                        endtag = m_end.group(1).upper().rstrip().lstrip()
                        if endtag == branchname:  # non-matching, raise exception
                            if DEBUG:
                                print(f"{line[0]:8d}   " * lvl + f"CLOSING {branchname}")
                            return [newbranch, False]  # return newborn child to parent
                        else:  # return new born to parent
                            raise DSTreeException(
                                f"FILE ENDED in [{branchname}]-section, started on line {opened},\n"
                                + f"  but found [{m_end.group(0)}] on line  {line[0]},\n  At tree level = {lvl}"
                            )
                    elif m_nxt:
                        nxttag = m_nxt.group(1).upper().rstrip().lstrip()
                        if nxttag == branchname:  # non-matching, raise exception
                            return (newbranch, True)  # return newborn child to parent
                        else:  # return new born to parent
                            raise DSTreeException(
                                f"FILE ENTERED [{branchname}]-section on line opened,\n"
                                + f"but found [{m_nxt.group(0)}] on line  {line[0]},\n  At tree level = {lvl}"
                            )
                    else:  # a new child was born ...
                        try:
                            has_next = True
                            while has_next:
                                newsubbranch, has_next = cls.__branch(tag, dmp, lvl + 1, line)
                                if tag not in newbranch:
                                    newbranch[tag] = []
                                newbranch[tag].append(newsubbranch)
                        except DSTreeException as dse:
                            raise dse
                else:
                    pass  # actions handling normal lines
                    if "txt" not in newbranch:
                        newbranch["txt"] = []  # stringlist
                    newbranch["txt"].append(dmpline.rstrip().lstrip())
            else:
                if lvl > 0:
                    raise DSTreeException(
                        f"FILE ENDED in [{branchname}]-section, started on line {opened}\n"
                        + f"  because the expected [END OF {branchname}] was not found!\n  At tree level = {lvl}"
                    )
                else:
                    return (
                        newbranch,
                        False,
                    )  # return but unfinished business (rase exception?)
