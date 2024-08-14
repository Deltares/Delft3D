import re

import src.utils.comparers.tree_comparer as TreeComparer

# ------------------------DSeriesComparer-constructor------------------------ #


class DSeriesComparer(TreeComparer.TreeComparer):
    def __init__(self) -> None:
        # call base class constructor
        TreeComparer.TreeComparer.__init__(self)
        self.skipped_keys.append("txt")
        return

        # the skipped keys should not be passed on in the next call of
        # compareTreePaths, but handled in compareThisNode.
        # Stop descending the tree!

    def parseNode(self, text):
        """Parses node as a dictionary."""
        try:
            lines = []
            for line in text:
                lines.append(line)
        except Exception:
            raise TreeComparer.TreeException("Could not parse block\n ")
        dictionary = {}
        keys = []
        values = []

        try:
            try:
                for i in range(len(lines)):
                    values.append(float(lines[i].strip()))
                    keys.append(str(i))
                    # strip comments enclosed by [] {} () from both
                    keys[i] = re.split(r"\s*[\(\[{]", keys[i])[0].strip()
                    values[i] = re.split(r"\s*[\(\[{]", values[i])[0].strip()
                    dictionary[keys[i]] = str(values[i])
            except:
                try:
                    i = 0
                    for line in lines:
                        if ":" in line:
                            split = line.split(":", 1)
                            str1 = split[0].strip()
                            str2 = split[1].strip()
                            kvPair = self.createKeyValuePair(str1, str2, dictionary)
                            # strip comments enclosed by [] {} () from both
                            kvPair[0] = re.split(r"\s*[\(\[{]", kvPair[0])[0].strip()
                            kvPair[1] = re.split(r"\s*[\(\[{]", kvPair[1])[0].strip()
                            dictionary[kvPair[0]] = kvPair[1]
                        elif "=" in line:
                            split = line.split("=", 1)
                            str1 = split[0].strip()
                            str2 = split[1].strip()
                            kvPair = self.createKeyValuePair(str1, str2, dictionary)
                            # strip comments enclosed by [] {} () from both
                            kvPair[0] = re.split(r"\s*[\(\[{]", kvPair[0])[0].strip()
                            kvPair[1] = re.split(r"\s*[\(\[{]", kvPair[1])[0].strip()
                            dictionary[kvPair[0]] = kvPair[1]
                        else:
                            dictionary[str(i)] = line
                            i += 1
                except Exception as e1:
                    raise e1
        except Exception as e:
            raise e
        return dictionary

    def buildTrees(self, file):
        # dmp and ref are file handles to respectively dump and reference
        lvl = 0
        line = [0]
        try:
            tree = self.branch("Test", file, lvl, line)
        except TreeComparer.TreeException as e:
            raise Exception("Tree not built" + ": " + e.message)
        return tree

    @classmethod
    def branch(cls, branchname, dmp, lvl, line):
        newbranch = {}
        opened = line[0]
        newbranch["block_start"] = [line[0]]
        newbranch["block_end"] = [-1]
        while True:
            dmpline = dmp.readline()
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
                            newbranch["block_end"] = [line[0]]
                            return [newbranch, False]  # return newborn child to parent
                        else:  # return new born to parent
                            raise TreeComparer.TreeException(
                                "FILE ENDED in ["
                                + branchname
                                + "]-section, started on line "
                                + "%d," % opened
                                + "\n  but found ["
                                + m_end.group(0)
                                + "] on line  "
                                + "%d," % line[0]
                                + "\n  At tree level = "
                                + "%d" % lvl
                            )
                    elif m_nxt:
                        nxttag = m_nxt.group(1).upper().rstrip().lstrip()
                        if nxttag == branchname:  # non-matching, raise exception
                            newbranch["block_end"] = [line[0]]
                            return newbranch, True  # return newborn child to parent
                        else:  # return new born to parent
                            raise TreeComparer.TreeException(
                                "FILE ENTERED ["
                                + branchname
                                + "]-section on line "
                                + "%d," % opened
                                + "\n  but found ["
                                + m_nxt.group(0)
                                + "] on line  "
                                + "%d," % line[0]
                                + "\n  At tree level = "
                                + "%d" % lvl
                            )
                    else:  # a new child was born ...
                        try:
                            has_next = True
                            while has_next:
                                newsubbranch, has_next = cls.branch(tag, dmp, lvl + 1, line)
                                if tag not in newbranch:
                                    newbranch[tag] = []
                                newbranch[tag].append(newsubbranch)
                        except TreeComparer.TreeException as dse:
                            raise dse
                else:
                    pass  # actions handling normal lines
                    if "txt" not in newbranch:
                        newbranch["txt"] = []  # stringlist
                    newbranch["txt"].append(dmpline.rstrip().lstrip())
            else:
                if lvl > 0:
                    raise TreeComparer.TreeException(
                        "FILE ENDED in ["
                        + branchname
                        + "]-section, started on line "
                        + "%d," % opened
                        + "\n  because the expected [END OF "
                        + branchname
                        + "] was not found!"
                        + "\n  At tree level = "
                        + "%d" % lvl
                    )
                else:
                    return (
                        newbranch,
                        False,
                    )  # return but unfinished business (raise exception?)
