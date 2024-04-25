#  (dump files, xml,json)
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2017

import os
import re
import sys
from typing import List, Tuple

import numpy as np

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger

# ------------------------TreeException-constructor------------------------ #


class TreeException(Exception):
    def __init__(self, m):
        sys.stderr.write("TreeComparer: " + m + "\n")
        self.message = m
        return

    def __str__(self):
        return self.message


# ------------------------TreeComparer-constructor------------------------ #


class TreeComparer(IComparer):
    """
    Compare two files with a data tree recursively,
       according to the configuration in file_check.
    input: left path (reference), right path (compare), file_check
    output: list of (file_check, parameter, file_check, ResultComparison) tuples
    """

    def __init__(self):
        self.skipped_keys = ["block_end", "block_start"]
        return

    # ------------------------Tree-comparison-methods------------------------ #

    def dumpRefTree(self, funit, janummer):
        dumpTreePaths(funit, self.reftree[0], "-", janummer)

    def dumpTestTree(self, funit, janummer):
        dumpTreePaths(funit, self.testtree[0], "-", janummer)

    def getBranchFromPath(self, br, path):
        """
        When given a full dictionary retrieve only the branch that refers to the path entered.
        :param br: full tree
        :param path: path to the branch that should be retrieved
        :return: the path found and the branch
        """
        elements = path.split(">")
        foundpath = ""
        for element in elements:
            if element > "":
                if "!" in element:
                    element, ndx = element.split("!")
                else:
                    ndx = 0
                if element in br:
                    if int(ndx) > 0:
                        foundpath = foundpath + ">" + element + "!" + ndx
                    else:
                        foundpath = foundpath + ">" + element
                    br = br[element][int(ndx)]
                else:
                    raise TreeException("Wrong path : " + path)
        return [foundpath, br]

    def compareTreePaths(
        self,
        testbranch,
        refbranch,
        parameter,
        pathstr,
        currentresults,
        ignored_parameters,
        logger: ILogger,
    ):
        """Compare trees recursively."""
        results = currentresults
        if (pathstr + ":") not in ignored_parameters:
            if testbranch.get("txt") is not None:
                if not (list(filter(None, testbranch.get("txt")))):
                    testbranch.pop("txt", None)
            if refbranch.get("txt") is not None:
                if not (list(filter(None, refbranch.get("txt")))):
                    refbranch.pop("txt", None)
            notInRef = set(testbranch.keys()) - set(
                refbranch.keys()
            )  # keys extra, added
            notInTest = set(refbranch.keys()) - set(
                testbranch.keys()
            )  # keys missing, removed
        else:
            notInRef = {}
            notInTest = {}

        if len(notInRef) > 0:
            missingblocksstring = " , ".join(list(notInRef))
            raise TreeException(
                "The reference file is missing blocks: " + missingblocksstring
            )
        elif len(notInTest) > 0:
            missingblocksstring = " , ".join(list(notInTest))
            raise TreeException(
                "The test file is missing blocks: " + missingblocksstring
            )
        else:
            try:
                results = self.compareThisNode(
                    testbranch,
                    refbranch,
                    pathstr,
                    parameter,
                    results,
                    ignored_parameters,
                    logger,
                )
                for (
                    key,
                    refvals,
                ) in refbranch.items():  # check from REFERENCE, key key->values
                    if key not in self.skipped_keys:
                        if (
                            key in testbranch
                        ):  # if this key is not in the TEST ... test fails
                            testvals = testbranch[
                                key
                            ]  # get values (which also can be branches)
                            if len(testvals) != len(
                                refvals
                            ):  # value list must have same length in test
                                raise TreeException(
                                    pathstr
                                    + "/"
                                    + key
                                    + ": Not the same size in ref and test"
                                )
                            else:
                                for testval, refval in zip(testvals, refvals):
                                    if key != "DATA" and key != "COLUMN INDICATION":
                                        self.compareTreePaths(
                                            testval,
                                            refval,
                                            parameter,
                                            pathstr + ">" + key,
                                            results,
                                            ignored_parameters,
                                            logger,
                                        )
            except TreeException as e:
                raise
        return results

    def getVarData(self, tree, path, varName):
        """
        From the full tree input this function first retrieves a branch according to the path specified as input.
        Then see if the branch is a table or a value.
        Return a value from that
        :param tree: full tree of dumpfile
        :param path: path to the branch
        :param varName: what is the name of the variable that we want to retrieve
        :return: value to be checked
        """
        # Retrieve branch
        try:
            pad, branch = self.getBranchFromPath(tree[0], path)
        except Exception as e:
            raise
        # Check if the value refers to a table
        if "::" not in varName:
            # retrieve the txt part of the branch
            test = branch["txt"]
            # Parse the values in the dictionary
            dictionary = self.parseNode(test)
            if varName in dictionary:
                return dictionary[varName]
            else:
                return []
        else:
            dictionary = self.getTable(branch)
            varRow = varName[0 : varName.index(":")]
            varCol = varName[varName.index(":") + 2 :]
            if varRow in dictionary:
                return dictionary[varRow][int(varCol) - 1]
            else:
                return []

    def compare(
        self,
        left_path,
        right_path,
        file_check: FileCheck,
        testcase_name,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        """
        This function will be called for all the values entered in the xml file.
        for them the trees will be compared and finally the parameters and end results will be output
        :param left_path: path to reference file
        :param right_path: path to test file
        :return: a list were it is shown if all the values were OK
        """
        filename = file_check.name
        self.test_path = right_path
        self.ref_path = left_path
        test_file = os.path.join(right_path, filename)
        ref_file = os.path.join(left_path, filename)
        results = []

        try:
            with open(test_file, "r") as ftest:
                self.testtree = self.buildTrees(ftest)
        except Exception as e:
            raise Exception("Cannot open tested file " + filename + " in " + right_path)

        try:
            with open(ref_file, "r") as fref:
                self.reftree = self.buildTrees(fref)
        except Exception:
            raise Exception("Cannot open reference file " + filename + " in " + left_path)

        parameters = file_check.parameters["parameters"]
        paramResults = []

        # define ignored parameters
        ignored_parameters = self.pullIgnored(parameters)

        # Work through the parameters of the tree
        for parameter in parameters:
            try:
                # Split the parameter to the name of the parameter
                branchpath, paramname = parameter.name.split(":")

                if (branchpath + ":") not in ignored_parameters:
                    # Get the branch of the test file
                    try:
                        pad, self.testbranch = self.getBranchFromPath(
                            self.testtree[0], branchpath
                        )
                    except:
                        raise Exception(
                            "Path " + branchpath + " not found in result !!"
                        )

                    # Get the branch of the reference file
                    try:
                        pad, self.refbranch = self.getBranchFromPath(
                            self.reftree[0], branchpath
                        )
                    except:
                        raise Exception(
                            "Path " + branchpath + " not found in reference !!"
                        )

                    # Compare the TreePaths with each other
                    newResults = self.compareTreePaths(
                        self.testbranch,
                        self.refbranch,
                        parameter,
                        pad,
                        [],
                        ignored_parameters,
                        logger,
                    )
                    # Append the results this contains every possible path in the Tree
                    results.extend(newResults)
            except Exception as e:
                raise
            # Return one result per entry in config file
            local_error = False
            # Create container that will hold all the values and append them in the paramResults
            end_result = ComparisonResult(error=local_error)
            end_result.result = "OK"
            # Go through all the results
            # If there is an ERROR or if the values are NOK then new result will be modified
            # Those wrong path are append in the end_result
            # And the coordinates of the block that fails
            for result in results:
                if result.result == "NOK":
                    end_result.result = "NOK"
                    if (
                        result.maxAbsDiffCoordinates
                        not in end_result.maxAbsDiffCoordinates
                    ):
                        end_result.maxAbsDiffCoordinates = result.maxAbsDiffCoordinates
                    if result.lineNumber != 0:
                        result.path = (
                            result.path + " (row: " + str(result.lineNumber) + ")"
                        )
                    if result.path not in end_result.path:
                        end_result.path.append(result.path)
                        parameter.location = result.path
                if result.error:
                    end_result.error = True
                    end_result.result = "ERROR"
                    if result.lineNumber != 0:
                        result.path = (
                            result.path + " (row: " + str(result.lineNumber) + ")"
                        )
                    if result.path not in end_result.path:
                        end_result.path.append(result.path)
            paramResults.append((testcase_name, file_check, parameter, end_result))
        return paramResults

    # @abstractmethod
    # def buildTrees(self, file):
    #     pass

    def pullIgnored(self, parameters: List[Parameter]):
        """
         Pulls the ignored parameters and appends them in a list according to their name
        :param parameters: all parameters read from xml
        :return: a list of the ignored parameters names
        """
        # loop through all the parameters
        ignoredparameters = []
        for parameter in parameters:
            if parameter.ignore:
                ignoredparameters.append(parameter.name)
        return ignoredparameters

    # -----------------------Node-comparison-methods----------------------- #

    def compareTableWithMissingColumn(
        self, resultslist, reftable, testtable, testbranch, refbranch, logger: ILogger
    ):
        """
        Finds missing column and returns updated list of results
        Log message of the tables were the column is added is also included
        :arg resultslist, reftable, testtable, testbranch, refbranch
        :return resultlist - a dictionary of ComparisonResult lists
        """

        # Create a container of result to store the error
        local_error = False
        columnresults = ComparisonResult(error=local_error)
        # Result is always NOK when a column is missing
        columnresults.result = "NOK"

        if reftable.__len__() < testtable.__len__():
            # Column was added to the table
            missingcolumns = list(set(testtable.keys()) - set(reftable.keys()))
            columnresults.maxAbsDiffCoordinates = (
                testbranch["block_start"][0],
                testbranch["block_end"][0],
            )
            logger.info(
                "\nColumn [%s] missing in reference. Block starts in line [%s] of the dumpfile"
                % (",".join(missingcolumns), testbranch["block_start"][0])
            )
        else:
            # Column was removed from the table
            missingcolumns = list(set(reftable.keys()) - set(testtable.keys()))
            columnresults.maxAbsDiffCoordinates = (
                refbranch["block_start"][0],
                refbranch["block_end"][0],
            )
            logger.info(
                "\nColumn [%s] missing in test. Block starts in line [%s] of the dumpfile"
                % (",".join(missingcolumns), refbranch["block_start"][0])
            )
        # Path is updated
        columnresults.path = missingcolumns
        resultslist.append(columnresults)
        return resultslist

    def compareThisNode(
        self,
        testbranch,
        refbranch,
        pathstr,
        parameter,
        resultlist,
        ignored_parameters,
        logger: ILogger,
    ):
        """
        Specific comparison of this node : floats, txt and tables
        :arg DSeriesComparer, testbranch, refbranch, parameter
        :return resultlist - a dictionary of ComparisonResult lists
        """
        # get the branch and the name
        branchpath, varname = parameter.name.split(":")
        local_error = False

        # Create a container of result
        result = ComparisonResult(error=local_error)

        # parse and compare table for this node
        # if the table does not exist this will return a None
        testtable = self.getTable(testbranch)
        reftable = self.getTable(refbranch)
        # In the case the tables exist
        if (reftable is not None) and (testtable is not None):
            # Check if their lengths are the same
            if reftable.__len__() == testtable.__len__():
                # Compare them as tables
                newresults = self.compareDataTables(
                    reftable, testtable, pathstr, parameter, logger
                )
                resultlist.extend(newresults)
            else:
                # Comparing tables where the length of the columns is not the same
                resultlist = self.compareTableWithMissingColumn(
                    resultlist, reftable, testtable, testbranch, refbranch, logger
                )
        # parse and compare non-table statements
        try:
            # check for txt block
            if refbranch.get("txt") is None:
                result.error = True
                result.path = branchpath + varname
                return resultlist
            ref = refbranch.get("txt")
            test = testbranch.get("txt")
            # filter out empty lines
            ref = list(filter(None, ref))
            test = list(filter(None, test))
            # Create dictionaries for the branches
            testdictionary = self.parseNode(test)
            refdictionary = self.parseNode(ref)

            # Check if the dictionaries are the same. The first step is to check that they have the same keys.
            # raise an exception when the two dictionaries do not have the same composition
            settest = set(testdictionary.keys()) - set(
                refdictionary.keys()
            )  # test - ref
            setref = set(refdictionary.keys()) - set(
                testdictionary.keys()
            )  # ref - test
            setCompareMessage = ""

            # If the parameters should be ignored do not call the function
            if (pathstr + ":") not in ignored_parameters:
                if settest > set():
                    setCompareMessage += "\nKeys [%s] missing in reference." % (
                        ",".join(settest)
                    )
                if setref > set():
                    setCompareMessage += "\nKeys [%s] missing in test." % (
                        ",".join(setref)
                    )
                if setCompareMessage:  # Make sure dictionaries have the same size
                    raise TreeException(
                        "Set of variables not equal in path:\n %s.%s"
                        % (pathstr, setCompareMessage)
                    )
                # Compare the dictionaries creates and append them to the results
                result = self.compareDictionary(
                    testdictionary, refdictionary, pathstr, parameter, logger
                )
                if result > []:
                    resultlist.extend(result)

        except Exception as e:
            raise TreeException("Could not compare values in path\n %s." % pathstr)

        return resultlist

    def createKeyValuePair(self, str1, str2, dict1):
        """
        Creates key-value pair to put into dictionary
        """
        i = 1
        j = 1
        try:
            float(str2)
            if str1 not in dict1:
                return [str1, str2]
            while str1 in dict1 and str1 + "_" + str(i) in dict1:
                i += 1
            return [str1 + "_" + str(i), str2]
        except:
            if str2 not in dict1:
                return [str2, str1]
            while str2 in dict1 and str2 + "_" + str(j) in dict1:
                j += 1
            return [str2 + "_" + str(j), str1]

    def getTable(self, br):
        """
        Check if current node contains a table
        if it does return it to the correct format
        :param br: The table in generic form
        :return: a dictionary of a table or None if there is no table
        """
        try:
            if ("COLUMN INDICATION" in br) and ("DATA" in br):
                tbldata = dict()
                colnames = []
                for colname in br["COLUMN INDICATION"][0]["txt"]:  # read column names
                    colnames.append(colname)
                    tbldata[colname] = np.array([])
                ncol = len(colnames)
                if br["DATA"][0].get("txt") is None:
                    return None
                for row in br["DATA"][0]["txt"]:  # read data rows
                    fields = self.rowToArray(row)
                    if len(fields) >= ncol:
                        for icol in range(ncol):
                            tbldata[colnames[icol]] = np.append(
                                tbldata[colnames[icol]], fields[icol]
                            )
                    elif (
                        row.split("  ")[-1].strip() == "Number of stages"
                        or row.split("  ")[-1].strip() == "Number of Layers"
                    ):
                        pass
                    else:
                        raise TreeException(
                            "Expecting at least %d fields," % ncol
                            + ", but found %d ... skipping line ..." % len(fields)
                        )
                return tbldata
            else:
                return None
        except:
            raise TreeException("Table not parseable")

    def rowToArray(self, ss):
        """
        Split a row into an array of values
        :param ss: strings
        :return: list of strings
        """
        a = []
        # In the case of multiple strings the list a will be empty
        if not a:
            try:
                ss2 = ss.lstrip().rstrip()
                begin = False
                end = False
                readnumber = False
                outstring = ""
                for element in ss2:
                    if begin and (element != "'"):
                        outstring += element
                    if end:
                        begin = False
                        end = False
                        a.append(outstring.strip())
                        outstring = ""

                    if (element == "'") and (not begin):
                        begin = True
                    elif (element == "'") and (not end):
                        end = True

                    if (not begin) and (not end) and (element.strip()):
                        # you are reading a number
                        readnumber = True
                        outstring += element
                    if readnumber and not element.strip():
                        readnumber = False
                        # A string without quotes can also come here
                        if TreeComparer.isFloat(outstring):
                            a.append(float(outstring))
                        else:
                            a.append(outstring.strip())
                        outstring = ""
                if TreeComparer.isFloat(outstring):
                    a.append(float(outstring))
                else:
                    a.append(outstring.strip())
            except:
                raise TreeException("Row of table is not parseable")
        return a

    def isFloat(value):
        """
        Test if value is float
        :param value: strings
        :return: boolean which is true if the input value is a float
        """
        try:
            float(value)
            return True
        except ValueError:
            return False

    def compareDictionary(
        self, testdictionary, refdictionary, pathstr, parameter, logger: ILogger
    ):
        """
        Compares the values form the dictionary made with the information held within a node
        :param testdictionary: input of test dictionary
        :param refdictionary: input of reference dictionary
        :param pathstr: The path refered in the dictionary but the top part of the path
        :param parameter: the general parameter as inputted from xml
        :return:
        """
        branchpath, varname = parameter.name.split(":")
        local_error = False
        results = []
        # Get all the keys of the dictionary
        for key in refdictionary.keys():
            if re.search(varname, key):
                # Get the values of ref and test
                refvalue = refdictionary[key]
                testvalue = testdictionary[key]
                # Make a container of results
                result = ComparisonResult(error=local_error)
                result.path = pathstr + ">" + key
                try:
                    if type(refvalue) is str and type(testvalue) is str:
                        # if they are floats
                        refvalue = float(refvalue)
                        testvalue = float(testvalue)
                    # The values are exactly the same
                    if refvalue == testvalue:
                        result.result = "OK"
                    # The values are not the same but they are within the Tolerances
                    elif abs(testvalue - refvalue) <= self.SetPythonCompatibility(
                        parameter.getToleranceAbsolute()
                    ) and abs(
                        (testvalue - refvalue) / refvalue
                    ) <= self.SetPythonCompatibility(
                        parameter.getToleranceRelative()
                    ):
                        result.result = "OK"
                    # The value is not the same and is above absolute Tolerances
                    elif abs(testvalue - refvalue) >= self.SetPythonCompatibility(
                        parameter.getToleranceAbsolute()
                    ):
                        result.maxAbsDiff = abs(testvalue - refvalue)
                        result.maxAbsDiffValues = (testvalue, refvalue)
                        message = (
                            "Absolute Error:   "
                            + "test = %12.6e     ref = %12.6e (%12.6e): %s"
                            % (testvalue, refvalue, result.maxAbsDiff, result.path)
                        )
                        logger.info(message)
                        result.result = "NOK"
                    # The value is not the same and is above relative Tolerances
                    else:
                        result.maxRelDiff = abs((testvalue - refvalue) / refvalue)
                        result.maxRelDiffValues = (testvalue, refvalue)
                        message = (
                            "Relative Error:   "
                            + "test = %12.6e     ref = %12.6e (%10.2f %%): %s"
                            % (
                                testvalue,
                                refvalue,
                                result.maxRelDiff * 100,
                                result.path,
                            )
                        )
                        logger.info(message)
                        result.result = "NOK"
                    results.append(result)
                except:
                    # if the values tested are strings then they are tested here for their equality
                    try:
                        if refvalue == testvalue:
                            result.result = "OK"
                        else:
                            result.result = "NOK"
                            results.append(result)
                    except:
                        local_error = True
                        result.error = local_error
                        results.append(result)
        return results

    def compareDataTables(
        self, reftable, testtable, pathstr, parameter: Parameter, logger: ILogger
    ):
        """
        Compares the values held within a table
        :param reftable:
        :param testtable:
        :param pathstr:
        :param parameter:
        :return:
        """
        branchpath, varname = parameter.name.split(":")
        results = []
        local_error = False
        columnNumber = 0
        # Go through all the keys of the table
        for key in reftable.keys():
            if re.search(varname, key):
                columnNumber += 1
                # Lists of values refering to each column of the table
                refvalue = reftable[key]
                testvalue = testtable[key]
                for i, ref_val in enumerate(refvalue):
                    # Create a container for the results
                    result = ComparisonResult(error=local_error)
                    result.lineNumber = i + 1
                    result.columnNumber = columnNumber
                    result.path = pathstr + ">" + key
                    # values equal
                    if ref_val == testvalue[i]:
                        result.result = "OK"
                    # values of absolute diff and relative diff within margins
                    else:
                        if isinstance(ref_val, str) and isinstance(testvalue[i], str):
                            message = (
                                f"Absolute Error:   test = {testvalue[i]}"
                                + f"     ref = {ref_val} ({False}): {result.path}({i:d})"
                            )
                            logger.info(message)
                            result.result = "NOK"
                        elif abs(testvalue[i] - ref_val) <= self.SetPythonCompatibility(
                            parameter.tolerance_absolute
                        ):
                            result.result = "OK"
                        elif abs(
                            (testvalue[i] - ref_val) / ref_val
                        ) <= self.SetPythonCompatibility(parameter.tolerance_relative):
                            result.result = "OK"
                        # absolute tolerance exceeded
                        elif abs(testvalue[i] - ref_val) > self.SetPythonCompatibility(
                            parameter.tolerance_absolute
                        ):
                            result.maxAbsDiff = abs(testvalue[i] - ref_val)
                            result.maxAbsDiffValues = (testvalue[i], ref_val)
                            message = (
                                f"Absolute Error:   test = {testvalue[i]:12.6e}"
                                + f"     ref = {ref_val:12.6e} ({result.maxAbsDiff:12.6e}): {result.path}({i:d})"
                            )
                            logger.info(message)
                            result.result = "NOK"
                        # relative tolerance exceeded
                        elif abs(
                            (testvalue[i] - ref_val) / ref_val
                        ) > self.SetPythonCompatibility(parameter.tolerance_relative):
                            result.maxRelDiff = abs((testvalue[i] - ref_val) / ref_val)
                            result.maxRelDiffValues = (testvalue[i], ref_val)

                            message = (
                                f"Relative Error:   test = {testvalue:12.6e}"
                                + f"     ref = {ref_val:12.6e} ({result.maxRelDiff * 100:10.2f}): {result.path}({i:d})"
                            )
                            logger.info(message)
                            result.result = "NOK"
                    results.append(result)
        return results

    def SetPythonCompatibility(self, value):
        """
        Depending on the version of python either accept None
        or choose a low value of Tolerance.
        :param value: the Tolerance
        :return: changed Tolerance
        """
        if value is None:
            value = -1
        return value

    # ------------------------End-Class-TreeComparer------------------------ #


def dumpTreePaths(funit, branch, pad, janummer):
    """
    Debugging, show me the full paths in the tree down from the specified branch.

    """
    for key, vals in branch.items():
        for val in vals:
            if type(val) == dict:
                linenrs = ""
                if janummer:
                    if ("block_start" in val) and ("block_end" in val):
                        linenrs = "(%d,%d)" % (
                            val["block_start"][0],
                            val["block_end"][0],
                        )
                funit.write(pad + ">" + key + "   " + linenrs + "\n")
                dumpTreePaths(funit, val, pad + ">" + key, janummer)
