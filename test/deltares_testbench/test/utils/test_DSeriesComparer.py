import os
import sys
from os.path import abspath, dirname, join

import pytest

from src.utils.comparers.d_series_comparer import DSeriesComparer

sys.path.insert(0, abspath(join(dirname(__file__), "..")))


class TestDSeriesComparer:
    def setup_method(self) -> None:
        self.python_version = sys.version_info[0]
        self.path_to_file = ""
        self.testroot = os.path.abspath(os.path.dirname(__file__))
        self.testdata = os.path.join(self.testroot, "data")
        self.lp = os.path.join(self.testdata, "left")
        self.rp = os.path.join(self.testdata, "right")

        # Parse the class to be tested as attribute
        self.comp = DSeriesComparer()
        pass

    def test_parseNode(self) -> None:
        # TEST 1
        # Set the inputs of first test
        list = ["", "", ""]

        # The output is an empty dictionary
        dictionary = {"0": "", "1": "", "2": ""}

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

        # TEST 2
        # Set the inputs of first test
        list = ["value1=1", "value2=2"]

        # The output is a dictionary
        dictionary = {"value1": "1", "value2": "2"}

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

        # TEST 3
        # Set the inputs of first test
        list = ["0 : value = withequals"]

        # The output is a dictionary
        dictionary = {"value = withequals": "0"}

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

        # TEST 4
        # Set the inputs of first test
        list = ["valueshere", "value1=1", "value2=2", "value3=3", "value4=4"]

        # The output is a dictionary
        dictionary = {
            "0": "valueshere",
            "value1": "1",
            "value2": "2",
            "value3": "3",
            "value4": "4",
        }

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

        # TEST 5
        # Set the inputs of first test
        list = ["value"]

        # The output is a dictionary
        dictionary = {"0": "value"}

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

        # TEST 6
        # Set the inputs of first test
        list = ["valueshere", "value1=1", "2 : value2"]

        # The output is a dictionary
        dictionary = {"0": "valueshere", "value1": "1", "value2": "2"}

        # Run the function
        dictionaryresult = DSeriesComparer.parseNode(self.comp, list)

        # Compare the results with the test
        assert dictionaryresult == dictionary

    def test_parseNode_Exception(self) -> None:
        # Random input that will raise exception
        list = 125

        # Run the function with  to catch exception
        with pytest.raises(Exception) as context:
            DSeriesComparer.parseNode(self.comp, list)

        # Check if the right exception is raised
        assert "Could not parse block\n " == str(context.value)

    def test_buildTrees(self) -> None:
        # Define the path for the fod file that will be tested
        filename = "Unit_test.fod"
        ref_file = os.path.join(self.lp, filename)
        # Open the .fod files
        fref = open(ref_file, "r")

        # Call the function
        reftree = DSeriesComparer.buildTrees(self.comp, fref)
        fref.close()

        # Check the equality with the target
        output = reftree[0]["DUMPFILE"][0]["DUMPFILE OUTPUT"][0]["VERIFICATION RESULTS"][0]["GLOBAL NEN RESULTS"][
            0
        ] == {
            "block_start": [1474],
            "block_end": [1479],
            "txt": [
                "0.063203 = wd1B",
                "0.009173 = Betad1B",
                "0.024783 = w2d",
                "0.003441 = Betad2",
            ],
        }

        # Check if it is True
        assert output

    def test_branch(self) -> None:
        # Set the inputs
        filename = "Unit_test_branch.fod"
        ref_file = os.path.join(self.lp, filename)
        fref = open(ref_file, "r")
        branchname = "Test"
        lvl = 0
        line = [0]

        # Call the function to be tested
        newbranch, next = DSeriesComparer.branch(branchname, fref, lvl, line)
        fref.close()

        # Setup the full branch
        testbranch = {
            "block_start": [10],
            "block_end": [35],
            "NEN AVERAGE PILE FACTORS": [
                {
                    "block_start": [11],
                    "block_end": [26],
                    "TABLE": [
                        {
                            "block_start": [12],
                            "block_end": [25],
                            "COLUMN INDICATION": [
                                {
                                    "block_start": [13],
                                    "block_end": [19],
                                    "txt": [
                                        "CptIndex",
                                        "AlphaSGemSg",
                                        "AlphaSGemClp",
                                        "AlphaPGem",
                                        "CptName",
                                    ],
                                }
                            ],
                            "DATA": [
                                {
                                    "block_start": [20],
                                    "block_end": [24],
                                    "txt": [
                                        "1  0.0060  0.0000  0.8000",
                                        "2  0.0060  0.0200  0.8000",
                                        "3  0.0060  0.0000  0.8000",
                                    ],
                                }
                            ],
                        }
                    ],
                }
            ],
            "txt": [""],
            "CALCULATION PARAMETERS BEARING PILES EC 7": [
                {
                    "block_start": [28],
                    "block_end": [34],
                    "txt": [
                        "Ksi3Used = 1.3000",
                        "Ksi4Used = 1.3000",
                        "GammaBUsed = 1.2000",
                        "GammaSUsed = 1.2000",
                        "IsKsi3Used = 0",
                    ],
                }
            ],
        }

        # Check if the branches are the same
        newbranch = newbranch["DUMPFILE"][0]
        assert newbranch == testbranch

    def test_branch_Exception(self) -> None:
        # Set the inputs
        filename = "Unit_test_branch.fod"
        ref_file = os.path.join(self.lp, filename)
        fref = open(ref_file, "r")
        branchname = "Test"

        # Set the wrong input
        lvl = 9
        line = [9]

        error = (
            "FILE ENDED in [Test]-section, started on line 9,\n  "
            "because the expected [END OF Test] was not found!\n  At tree level = 9"
        )
        # Check the equality with the target
        with pytest.raises(Exception) as context:
            DSeriesComparer.branch(branchname, fref, lvl, line)
        fref.close()

        # Check if the right exception is raised
        assert error == str(context.value)
