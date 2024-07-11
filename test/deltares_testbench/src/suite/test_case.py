"""
Description: Test Case Handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import copy
import os
import time
from typing import Dict, List, Tuple, ClassVar

from src.config.test_case_config import TestCaseConfig
from src.suite.program import Program
from src.utils.logging.i_logger import ILogger
from src.utils.paths import Paths


# Test case handler (compare or reference)
class TestCase(object):
    __errors = []
    __test__: ClassVar[bool] = False

    # constructor
    # input: test case configuration
    def __init__(self, config: TestCaseConfig, logger: ILogger):
        self.__config = config
        self.__logger = logger
        self.__maxRunTime: float = self.__config.max_run_time
        self.__programs: List[Tuple[int, Program]] = []

        logger.debug(
            f"Initializing test case ({self.__config.name}), max runtime : {str(self.__maxRunTime)}"
        )

        self.__config.run_file_name = os.path.join(
            self.__config.absolute_test_case_path, "_tb3_char.run"
        )
        refrunfile = os.path.join(
            config.absolute_test_case_reference_path, "_tb3_char.run"
        )

        if os.path.exists(refrunfile):
            refruntime = self.__findCharacteristicsRunTime__(refrunfile)
            if refruntime:
                self.__config.ref_run_time = refruntime
                if not self.__config.overrule_ref_max_run_time:
                    # set maxRunTime to 1.5 * reference runtime and add a few seconds (some systems start slow)
                    # The variation in runtimes vary a lot (different machines, other processes)
                    self.__maxRunTime = refruntime * 1.5 + 10.0
                    logger.info(
                        f"Overwriting max run time via reference _tb3_char.run ({str(self.__maxRunTime)})"
                    )

        self.__maxRunTime = max(self.__maxRunTime, 120.0) * 5.0 + 300.0
        logger.debug(f"maxRunTime increased to {str(self.__maxRunTime)}")

    def run(self, programs: List[Program]):
        """execute a Test Case
        [remark] execution does not throw errors, these can be retrieved from
        getErrors()

        Args:
            programs (List[Program]): list of programs

        Raises:
            RuntimeError: On time out
        """
        # prepare the programs for running

        logger = self.__logger
        self.__initializeProgramList__(programs)

        logger.debug("Starting test case")

        # prepare presets for testbench run file
        input_files, size = self.__get_initial_state()

        start_time = time.time()
        logger.debug(f"Test case start time {str(time.ctime(int(start_time)))}")

        # execute all programs, subprocess
        for program in self.__programs:
            program[1].run(logger)

        # create testbench run file
        elapsed_time = time.time() - start_time
        self.__config.run_time = elapsed_time
        logger.debug(f"Test case elapsed time {str(elapsed_time)}")
        logger.debug("Creating _tb3_char.run for test case")

        with open(self.__config.run_file_name, "w") as runfile:
            runfile.write("Start_size:" + str(size) + "\n")
            runfile.write("Runtime:" + str(elapsed_time) + "\n")
            for allfile in os.listdir(self.__config.absolute_test_case_path):
                # collect all added and changed files in the working directory (after running, compare to initial list)
                if allfile not in {}.fromkeys(input_files, 0):
                    runfile.write("Output_added:" + str(allfile) + "\n")
                    size = size + os.path.getsize(
                        os.path.join(self.__config.absolute_test_case_path, allfile)
                    )
                else:
                    ftime = os.path.getmtime(
                        os.path.join(self.__config.absolute_test_case_path, allfile)
                    )
                    if ftime != input_files[allfile]:
                        runfile.write("Output_changed:" + str(allfile) + "\n")
            runfile.write("End_size:" + str(size) + "\n")

    def __get_initial_state(self) -> Tuple[Dict[str, float], int]:
        inputfiles: Dict[str, float] = {}
        size: int = 0

        # collect all initial files in the working directory before running
        for infile in os.listdir(self.__config.absolute_test_case_path):
            inputfiles[infile] = os.path.getmtime(
                os.path.join(self.__config.absolute_test_case_path, infile)
            )
            size = size + os.path.getsize(
                os.path.join(self.__config.absolute_test_case_path, infile)
            )

        return inputfiles, size

    # get errors from Test Case
    # output: list of Errors (type), can be None
    def getErrors(self):
        return self.__errors

    def __initializeProgramList__(self, programs: List[Program]):
        """prepare programs from configuration"""

        # programs are loaded by the manager
        shell_arguments = " ".join(self.__config.shell_arguments)
        shell = self.__config.shell

        for program_config in self.__config.program_configs:
            # get the copy of the original program
            program: Program = next(
                p for p in programs if p.name == program_config.name
            )
            program_copy: Program = copy.deepcopy(program)

            # Combine the program workdir with the testcase workdir
            if program_config.working_directory:
                program_config.working_directory = Paths().mergePathElements(
                    self.__config.absolute_test_case_path,
                    program_config.working_directory,
                )
            else:
                program_config.working_directory = self.__config.absolute_test_case_path

            # overwrite run configuration with given overrides
            program_config.shell_arguments = shell_arguments
            program_config.shell = shell
            program_config.case_name = self.__config.name
            program_copy.overwriteConfiguration(program_config)

            # add runner sequence number and runner configuration to local storage
            self.__programs.append((program_config.sequence, program_copy))

    # retrieve runtime or none from _tb3_char.run file
    # input: path to _tb3_char.run file
    # output: actual runtime value (float)
    def __findCharacteristicsRunTime__(self, filename):
        with open(filename) as f:
            retval = None
            for line in f:
                if "Runtime:" in line:
                    _, value = line.split(":")
                    retval = float(value)
                    break
            return retval
