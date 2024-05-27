import os
import shutil
import sys
from os.path import abspath, dirname, isfile, join
from test.utils.test_logger import TestLogger

import pytest

from src.suite.test_bench_settings import TestBenchSettings
from src.utils.comparers.d_series_benchmark_comparer import DSeriesBenchmarkComparer
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.xml_config_parser import XmlConfigParser

sys.path.insert(0, abspath(join(dirname(__file__), "..")))


class TestDSeriesBenchmarkComparer:
    def setup_method(self):
        self.path_to_file = ""
        # Setting up the unit testing
        # Defining settings that will be used commonly
        # First import the class to be tested and make it an attribute
        self.testroot = abspath(os.path.dirname(__file__))
        self.testdata = join(self.testroot, "data")
        self.lp = join(self.testdata, "left")
        self.rp = join(self.testdata, "right")
        self.comp = DSeriesBenchmarkComparer()

        # Parse the xml file.
        # This is done to point to a specific file
        xmlcp = XmlConfigParser()
        logger = ConsoleLogger(LogLevel.DEBUG)
        settings = TestBenchSettings()
        settings.config_file = join(self.testdata, "Unit_test.xml")
        settings.credentials.name = "commandline"
        settings.local_paths, settings.programs, settings.configs = xmlcp.load(
            settings, logger
        )
        file = settings.configs

        # The first file that is going to be checked passed as attribute
        self.file_check = file[0].checks[0]
        pass

    def test_compare_No_csv_file(self):
        # Testing if the correct exception is raised when no csv file is found
        # define the inputs for the function
        ref_path = self.lp
        test_path = self.rp
        testcase_name = "Testing_all_the_fod_file"

        # Run the function
        with pytest.raises(Exception) as context:
            DSeriesBenchmarkComparer.compare(
                self.comp, ref_path, test_path, self.file_check, testcase_name
            )
            # Check if the correct exception is raised
            assert "Cannot open csv-file " + join(self.rp, "Unit_test.csv") == str(
                context.value
            )

    def test_compare(self):
        # Import the needed the packages
        # Delete files from directory so that they don't interfere with other tests
        lfile = join(self.lp, "Unit_test_empty.csv")
        if isfile(lfile):
            os.remove(lfile)
        rfile = join(self.rp, "Unit_test_empty.csv")
        if isfile(rfile):
            os.remove(rfile)

        # Set the paths to the cvs's files
        testcase_name = "Testing_all_the_fod_file"

        # Copy the Unit_test.csv files so that they can be read
        shutil.copy2(
            join(self.testdata, "Unit_test.csv"), join(self.lp, "Unit_test.csv")
        )
        shutil.copy2(
            join(self.testdata, "Unit_test.csv"), join(self.rp, "Unit_test.csv")
        )

        logger = TestLogger()
        # Run the function to be tested
        param_results = DSeriesBenchmarkComparer.compare(
            self.comp, self.lp, self.rp, self.file_check, testcase_name, logger
        )

        # Delete the files so that they don't interfere with the
        os.remove(join(self.lp, "Unit_test.csv"))
        os.remove(join(self.rp, "Unit_test.csv"))

        # Check if the result file is created
        assert os.path.isfile(join(self.rp, "param_results_Unit_test.csv"))

        # Set output as True this will be changed through an iterative process if it is not true
        output = True
        # Pass as list all the parameters that are in the csv file
        test = [
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::1",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::1",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::4",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::4",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::2",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::2",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::5",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|Max bearing capacity foundation Fr;fund;max;d",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::5",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::3",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::3",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxShaft::6",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS>MAX SHAFT AND POINT>TABLE|MaxPoint::6",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|Fr_max_schacht_d_1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|Fr_max_punt_d_1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|Max load on foundation Fs_rep",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>GLOBAL NEN RESULTS|wd1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>DEMANDS NEN-EN|wreq1b",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|spunt_d_1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|sel_d1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|s2_d1B",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>GLOBAL NEN RESULTS|w2d",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>DEMANDS NEN-EN|wreq2",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|spunt_d_2",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|sel_d2",
            ">DUMPFILE>DUMPFILE OUTPUT>VERIFICATION RESULTS>NEN PILE RESULTS|s2_d2",
        ]

        # Through a loop go through all the values check that their equality is True and that the name exists in the csv
        for parameter in param_results:
            if parameter[3].result != "OK":
                output = False
            if parameter[2].name not in test:
                output = False

        # Check if all the values are read and if every value exists
        assert output
        assert len(test) == len(param_results)

    def test_compareBenchmark_OK(self):
        # Set inputs in this case there should be no errors
        # Define the variable list as a dictionary
        varlist = [
            {
                "variable": "MaxShaft::1",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 1.0,
            },
            {
                "variable": "MaxPoint::1",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 2.0,
            },
            {
                "variable": "MaxShaft::2",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 3.0,
            },
            {
                "variable": "MaxPoint::2",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 4.0,
            },
            {
                "variable": "MaxShaft::3",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 5.0,
            },
            {
                "variable": "MaxPoint::3",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 6.0,
            },
            {
                "variable": "Parameter1",
                "path": ">DUMPFILE>CALCULATION PARAMETERS",
                "value": 1.3,
            },
            {
                "variable": "Parameter2",
                "path": ">DUMPFILE>CALCULATION PARAMETERS",
                "value": 1.4,
            },
            {"variable": "Parameter3", "path": ">DUMPFILE>DEMANDS", "value": 0.06},
            {"variable": "Parameter4", "path": ">DUMPFILE>DEMANDS", "value": 0.02},
        ]
        # Inputs of path and file
        testcase = "comparebenchmarks.fod"

        # Setting the absolute and relative Tolerance
        self.comp.absTol_global = 0.0005
        self.comp.relTol_global = 0.0005
        # Call the function to be tested
        results = DSeriesBenchmarkComparer.compareBenchmark(
            self.comp, varlist, self.lp, testcase
        )

        # The output
        testoutput = {
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::1": {
                "valueOK": "OK",
                "result value": 1.0,
                "benchmark value": 1.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::1": {
                "valueOK": "OK",
                "result value": 2.0,
                "benchmark value": 2.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::2": {
                "valueOK": "OK",
                "result value": 3.0,
                "benchmark value": 3.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::2": {
                "valueOK": "OK",
                "result value": 4.0,
                "benchmark value": 4.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::3": {
                "valueOK": "OK",
                "result value": 5.0,
                "benchmark value": 5.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::3": {
                "valueOK": "OK",
                "result value": 6.0,
                "benchmark value": 6.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>CALCULATION PARAMETERS|Parameter1": {
                "valueOK": "OK",
                "result value": 1.3,
                "benchmark value": 1.3,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>CALCULATION PARAMETERS|Parameter2": {
                "valueOK": "OK",
                "result value": 1.4,
                "benchmark value": 1.4,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>DEMANDS|Parameter3": {
                "valueOK": "OK",
                "result value": 0.06,
                "benchmark value": 0.06,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
            ">DUMPFILE>DEMANDS|Parameter4": {
                "valueOK": "OK",
                "result value": 0.02,
                "benchmark value": 0.02,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": 0.0,
                "absolute difference": 0.0,
            },
        }

        # Check if they are equal
        assert results == testoutput

    def test_compareBenchmark_Exception1(self):
        # Set inputs in this case there should be no errors
        # Define the variable list as a dictionary
        varlist = [
            {
                "variable": "MaxShaft::1",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 1.0,
            }
        ]
        # Inputs of path and file
        testcase = "thisdoesnotexist.fod"

        # Run function to get exception
        with pytest.raises(Exception) as context:
            DSeriesBenchmarkComparer.compareBenchmark(
                self.comp, varlist, self.rp, testcase
            )

        # Check if the exception raised is what expected
        assert "Cannot open tested file thisdoesnotexist.fod in " + self.rp == str(
            context.value
        )

    def test_compareBenchmark_Exception2(self):
        # Set inputs in this case there should be no errors
        # Define the variable list as a dictionary
        varlist = None
        # Inputs of path and file
        testcase = "comparebenchmarks.fod"

        # Run function
        result = DSeriesBenchmarkComparer.compareBenchmark(
            self.comp, varlist, self.lp, testcase
        )

        # Check if the exception raised is what expected
        assert {} == result

    def test_compareBenchmark_NOK(self):
        # Set inputs in this case there should be no errors
        # Define the variable list as a dictionary
        varlist = [
            {
                "variable": "MaxShaft::1",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "MaxPoint::1",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "MaxShaft::2",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "MaxPoint::2",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "MaxShaft::3",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "MaxPoint::3",
                "path": ">DUMPFILE>MAX SHAFT AND POINT>TABLE",
                "value": 999.0,
            },
            {
                "variable": "Parameter1",
                "path": ">DUMPFILE>CALCULATION PARAMETERS",
                "value": 999.0,
            },
            {
                "variable": "Parameter2",
                "path": ">DUMPFILE>CALCULATION PARAMETERS",
                "value": 999.0,
            },
            {"variable": "Parameter3", "path": ">DUMPFILE>DEMANDS", "value": 999.0},
            {"variable": "Parameter4", "path": ">DUMPFILE>DEMANDS", "value": 999.0},
        ]
        # Inputs of path and file
        testcase = "comparebenchmarks.fod"

        # Setting the absolute and relative Tolerance
        self.comp.absTol_global = 0.0005
        self.comp.relTol_global = 0.0005
        # Call the function to be tested
        results = DSeriesBenchmarkComparer.compareBenchmark(
            self.comp, varlist, self.lp, testcase
        )

        # The output
        testoutput = {
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::1": {
                "valueOK": "NOK",
                "result value": 1.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 1.0) / 999.0,
                "absolute difference": 999.0 - 1.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::1": {
                "valueOK": "NOK",
                "result value": 2.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 2.0) / 999.0,
                "absolute difference": 999.0 - 2.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::2": {
                "valueOK": "NOK",
                "result value": 3.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 3.0) / 999.0,
                "absolute difference": 999.0 - 3.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::2": {
                "valueOK": "NOK",
                "result value": 4.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 4.0) / 999.0,
                "absolute difference": 999.0 - 4.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxShaft::3": {
                "valueOK": "NOK",
                "result value": 5.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 5.0) / 999.0,
                "absolute difference": 999.0 - 5.0,
            },
            ">DUMPFILE>MAX SHAFT AND POINT>TABLE|MaxPoint::3": {
                "valueOK": "NOK",
                "result value": 6.0,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 6.0) / 999.0,
                "absolute difference": 999.0 - 6.0,
            },
            ">DUMPFILE>CALCULATION PARAMETERS|Parameter1": {
                "valueOK": "NOK",
                "result value": 1.3,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 1.3) / 999.0,
                "absolute difference": 999.0 - 1.3,
            },
            ">DUMPFILE>CALCULATION PARAMETERS|Parameter2": {
                "valueOK": "NOK",
                "result value": 1.4,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 1.4) / 999.0,
                "absolute difference": 999.0 - 1.4,
            },
            ">DUMPFILE>DEMANDS|Parameter3": {
                "valueOK": "NOK",
                "result value": 0.06,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 0.06) / 999.0,
                "absolute difference": 999.0 - 0.06,
            },
            ">DUMPFILE>DEMANDS|Parameter4": {
                "valueOK": "NOK",
                "result value": 0.02,
                "benchmark value": 999.0,
                "relative tolerance": 0.0005,
                "absolute tolerance": 0.0005,
                "relative difference": (999.0 - 0.02) / 999.0,
                "absolute difference": 999.0 - 0.02,
            },
        }

        # Check if they are equal
        assert results == testoutput

    def test_compare_empty(self):
        # Import packages
        import os
        import shutil

        # Reading a different csv file a new xml file has to be imported
        # Parse the xml file.
        xmlcp = XmlConfigParser()
        logger = ConsoleLogger(LogLevel.DEBUG)
        settings = TestBenchSettings()
        settings.config_file = join(self.testdata, "Unit_test_empty_file.xml")
        settings.credentials.name = "commandline"
        settings.local_paths, settings.programs, settings.configs = xmlcp.load(
            settings, logger
        )
        file = settings.configs
        # The file to be checked
        file_check = file[0]._TestCaseConfig__checks[0]

        # Define paths to references and tests
        testcase_name = "Testing_all_the_fod_file"

        # Copy files in the paths of reference and test
        shutil.copy2(
            join(self.testdata, "Unit_test_empty.csv"),
            join(self.lp, "Unit_test_empty.csv"),
        )
        shutil.copy2(
            join(self.testdata, "Unit_test_empty.csv"),
            join(self.rp, "Unit_test_empty.csv"),
        )

        logger = TestLogger()

        # Run the file for catching the exceptions
        with pytest.raises(Exception) as context:
            DSeriesBenchmarkComparer.compare(
                self.comp, self.lp, self.rp, file_check, testcase_name, logger
            )

        # Check if the correct exception is raised
        assert "No variables in CSV-file to compare !" == str(context.value)

        # Delete files from directory so that they don't interfere with other tests
        os.remove(join(self.lp, "Unit_test_empty.csv"))
        os.remove(join(self.rp, "Unit_test_empty.csv"))

    def test_parse_params(self):
        # Define the name of the csv file to be tested
        csvfilename = join(self.lp, "parse_params.csv")

        # Call the function to be tested
        varlist = DSeriesBenchmarkComparer.parse_params(self.comp, csvfilename)

        # Define the dictionary of the values
        vartest = [
            {"variable": "ValueName::1", "path": ">THIS>IS>THE>PATH", "value": 999.0},
            {
                "variable": "ValueName",
                "path": ">THIS>IS>THE>PATH",
                "value": 999.0,
                "absTol": 0.5,
            },
            {"variable": "ValueName::1", "path": ">THIS>IS>THE>PATH", "value": -999.0},
        ]

        # Check their equality
        assert varlist == vartest

    def test_parse_params_no_file(self):
        # Define the name of the csv file to be tested
        csvfilename = join(self.testdata, "filethatdoesnotexist.csv")

        # Call function but with with so that it catches all the exceptions
        with pytest.raises(Exception) as context:
            DSeriesBenchmarkComparer.parse_params(self.comp, csvfilename)

        # Check if the correct exceprion is raised
        assert "Cannot open csv-file " + csvfilename == str(context.value)

    def test_compute_abs_difference_between_strings(self):
        dsbc = DSeriesBenchmarkComparer()
        # only typo
        assert 1 == DSeriesBenchmarkComparer.compute_abs_difference_between_strings(
            dsbc, "Sand", "Pand"
        )
        # only length
        assert 2 == DSeriesBenchmarkComparer.compute_abs_difference_between_strings(
            dsbc, "Sand", "Sandaa"
        )
        # typo and length
        assert 2, DSeriesBenchmarkComparer.compute_abs_difference_between_strings(
            dsbc, "Sand", "Panda"
        )

    def test_split_lines_ignoring_quotes(self):
        dsbc = DSeriesBenchmarkComparer()
        assert [
            "Sand",
            "Clay",
            "Peat",
        ] == DSeriesBenchmarkComparer.split_lines_ignoring_quotes(
            dsbc, "Sand,Clay,Peat"
        )
        assert [
            "Sand",
            '"Clay,Peat"',
        ] == DSeriesBenchmarkComparer.split_lines_ignoring_quotes(
            dsbc, 'Sand,"Clay,Peat"'
        )
