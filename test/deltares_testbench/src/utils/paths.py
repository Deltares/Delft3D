"""
Description: Path helper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import os
import re
from typing import List


# Path helpers
class Paths(object):
    # split network path in server, folder and rest with os.sep
    # input: path
    # output: server part, folder part, left over part
    def splitNetworkPath(self, path):
        lfrom = path
        bws = "\\"
        if len(lfrom.split(bws)) > 2:
            while lfrom.find(bws) == 0:
                lfrom = lfrom[1:]
            while lfrom.endswith(bws):
                lfrom = lfrom[0 : lfrom.rfind(bws)]
            nps = lfrom.split(bws)
        fws = "/"
        if len(lfrom.split(fws)) > 2:
            while lfrom.find(fws) == 0:
                lfrom = lfrom[1:]
            while lfrom.endswith(fws):
                lfrom = lfrom[0 : lfrom.rfind(fws)]
            nps = lfrom.split(fws)
        server = nps[0]
        folder = nps[1]
        rest = ""
        for i in range(2, len(nps)):
            rest = rest + nps[i] + os.sep
        return server, folder, rest

    # check if a given path string is an actual path
    # input: path string
    # output: boolean
    def isPath(self, path):
        if "/" in path:
            if path.startswith("/") and path.count("/") == 1:
                return False
            if not re.match(r"^[A-Za-z][A-Za-z]+://", path):
                return True
        if "\\" in path:
            return True
        return False

    # check if the given path is an absolute path
    # input: path string
    # output: boolean
    def isAbsolute(self, path):
        if path[0] == "/" or re.match(r"^[A-Za-z]:", path):
            return True
        return False

    # ensure valid local system path
    # input: some local path string
    # output: valid system path
    def rebuildToLocalPath(self, path: str):
        # if path starts with a /, put that in result; it will be removed during splitting
        if path == "":
            return path
        if path[0] == "/":
            result = "/"
        else:
            result = ""
        # split in windows paths
        wpes = self.convertAbsoluteToLiteral(path).split("\\")
        for wpe in wpes:
            # split in linux path elements
            lpes = wpe.split("/")
            for lpe in lpes:
                if re.match(r"^[A-Za-z]:", lpe):
                    lpe = lpe + "\\"
                result = os.path.join(result, lpe)
        return result

    # convert a path string containing \ -> \\
    def convertAbsoluteToLiteral(self, path):
        # convert double backslash to single
        conv = re.sub(r"(\\\\)", r"\\", path)
        return re.sub(r"(?<!\\)+\\(?!\\)+", r"\\", conv)

    # assumes left is correct notation
    # input: left part and right part of path string
    # output: merged path assuming left part of string is correct notation
    def mergePathElements(self, left, right):
        fws = "/"
        bws = "\\"

        # Make sure that the handlers can accept URIs that contain spaces.
        if "://" in left:
            left = left.replace(" ", "%20")
            right = right.replace(" ", "%20")

        # remove ending slashes from left part
        tl = left
        tr = right
        while tl.endswith(fws):
            tl = tl[0 : tl.rfind(fws)]
        while tl.endswith(bws):
            tl = tl[0 : tl.rfind(bws)]
        # remove leading slashes from right part
        while tr.find(fws) == 0:
            tr = tr[1:]
        while tr.find(bws) == 0:
            tr = tr[1:]
        # if left is forward slashed
        if left.find(fws) >= 0:
            # invert backslashed if they exist in right
            return tl + fws + tr.replace(bws, fws)
        # if left is backward slashed
        if left.find(bws) >= 0:
            # invert forward slashed if they exist in right
            return tl + bws + tr.replace(fws, bws)
        # if left has no slashes and right is forward slashed
        if right.find(fws) >= 0:
            # put a forwards slash between left and right
            return tl + fws + tr
        # if left is backward slashed
        if right.find(bws) >= 0:
            # put a backwards slash between left and right
            return tl + bws + tr
        # No slashes in left or right, choose something

        return tl + fws + tr

    # merge path parts
    # input: left and rest (variable args)
    # output: string formatted on left as origin
    def mergeFullPath(self, left, *args):
        result = left
        for a in args:
            result = self.mergePathElements(result, a)
        return result

    # find all sub directory paths from a given path
    # input: root path to search from
    #        string identifying paths that should be excluded
    # output: list of absolute paths
    def findAllSubFolders(self, root, excludePathsContaining):
        if excludePathsContaining == "":
            return [os.path.abspath(x[0]) for x in os.walk(root)]
        else:
            return [
                os.path.abspath(x[0])
                for x in os.walk(root)
                if excludePathsContaining not in str(x[0])
            ]

    # find all files in all sub directories from a given path
    # input: root path to search from
    # output: list of file names
    def findAllSubFiles(self, root) -> List[str]:
        retval = []
        for subdir in self.findAllSubFolders(root, ""):
            prefix = subdir.replace(os.path.abspath(root), "")
            prefix = self.rebuildToLocalPath(prefix)
            retval.extend(
                [
                    os.path.join(prefix, f)
                    for f in os.listdir(subdir)
                    if os.path.isfile(os.path.join(subdir, f))
                ]
            )
        # Remove leading/trailing slashes; they mess up the comparison
        for index, rv in enumerate(retval):
            retval[index] = rv.strip("/\\")
        return retval
