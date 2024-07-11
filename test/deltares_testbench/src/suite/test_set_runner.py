"""
Description: Manager for running test case sets
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import multiprocessing
import os
import sys
from abc import ABC, abstractmethod
from datetime import datetime, timedelta
from multiprocessing.pool import AsyncResult
from typing import Iterable, List, Optional

from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_failure import TestCaseFailure
from src.config.types.handler_type import HandlerType
from src.config.types.mode_type import ModeType
from src.config.types.path_type import PathType
from src.suite.program import Program
from src.suite.run_data import RunData
from src.suite.test_bench_settings import TestBenchSettings
from src.suite.test_case import TestCase
from src.suite.test_case_result import TestCaseResult
from src.utils.common import log_header, log_separator, log_sub_header
from src.utils.errors.test_bench_error import TestBenchError
from src.utils.handlers.handler_factory import HandlerFactory
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.logging.i_logger import ILogger
from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType
from src.utils.paths import Paths


class TestSetRunner(ABC):
    """Run test cases in reference or compare mode"""

    def __init__(self, settings: TestBenchSettings, logger: IMainLogger) -> None:
        self.__settings = settings
        self.__logger = logger
        self.__duration = None
        self.programs: List[Program] = []
        self.skip_download = settings.skip_download
        self.finished_tests: int = 0

    @property
    def settings(self) -> TestBenchSettings:
        """Settings used for running tests

        Returns:
            TestBenchSettings: Used test settings
        """
        return self.__settings

    @property
    def duration(self) -> Optional[timedelta]:
        """Time it took to run the testbench

        Returns:
            Optional[timedelta]: elapsed time
        """
        return self.__duration

    def run(self):
        """Run test cases to generate reference data"""
        start_time = datetime.now()

        try:
            self.programs = list(self.__update_programs())
        except Exception:
            if self.__settings.teamcity:
                sys.stderr.write("##teamcity[testStarted name='Update programs']\n")
                sys.stderr.write("##teamcity[testFailed name='Update programs' message='Exception occurred']\n")

        self.__download_dependencies()
        log_sub_header("Running tests", self.__logger)

        results = self.run_tests_in_parallel() if self.__settings.parallel else self.run_tests_sequentially()

        log_separator(self.__logger, char="-", with_new_line=True)

        if results:
            self.show_summary(results, self.__logger)
        elif len(self.settings.configs_from_xml) == 0:
            self.__logger.warning("No testcases were loaded from the xml.")
        elif len(self.settings.configs_to_run) == 0 and self.settings.filter:
            self.__logger.warning(f"No testcases where found to run after applying the filter: {self.settings.filter}.")

        self.__duration = datetime.now() - start_time

    def run_tests_sequentially(self) -> List[TestCaseResult]:
        """Runs the test configurations sequentially and
        returns the results

        Returns:
            List[TestCaseResult]: list of test results
        """
        n_testcases = len(self.__settings.configs_to_run)
        results: List[TestCaseResult] = []

        for i_testcase, config in enumerate(self.__settings.configs_to_run):
            run_data = RunData(i_testcase + 1, n_testcases)

            try:
                result = self.run_test_case(config, run_data)
            except Exception as exception:
                self.__log_failed_test(exception)
                continue

            self.__log_successful_test(result)
            results.append(result)

        return results

    def run_tests_in_parallel(self) -> List[TestCaseResult]:
        """Runs the test configurations in parallel and
        returns the results

        Returns:
            List[TestCaseResult]: list of test results
        """
        n_testcases = len(self.__settings.configs_to_run)

        config_process_count = sum(config.process_count for config in self.__settings.configs_to_run)
        max_processes = min(config_process_count, multiprocessing.cpu_count())
        self.__logger.info(f"Creating {max_processes} processes to run test cases on.")
        process_manager = multiprocessing.Manager()

        with multiprocessing.Pool(processes=max_processes) as pool:
            self.finished_tests = 0

            result_futures: List[AsyncResult] = []
            in_use = process_manager.Value("i", 0)
            idle_process = process_manager.Condition()

            for i_testcase, config in enumerate(self.__settings.configs_to_run):
                run_data = RunData(i_testcase + 1, n_testcases)

                with idle_process:
                    while in_use.value + config.process_count > max_processes:
                        idle_process.wait()
                    in_use.value += config.process_count

                config_result_future = pool.apply_async(
                    self.run_test_case,
                    [config, run_data, in_use, idle_process],
                    callback=self.__log_successful_test,
                    error_callback=self.__log_failed_test,
                )

                result_futures.append(config_result_future)

            pool.close()
            pool.join()

            results: List[TestCaseResult] = []
            for result in result_futures:
                results.append(result.get())
        return results

    def run_test_case(
        self, config: TestCaseConfig, run_data: RunData, in_use=None, idle_process=None
    ) -> TestCaseResult:
        """Runs one test configuration (in a separate process)

        Args:
            config (TestCaseConfig): configuration to run
            run_data (RunData): Data related to the test run
            in_use (Integer): Amount of processes that are currently in use with testcases
            idle_process (Condition): Sends a notification to evaluate available cores for new testcase
        """
        logger = self.__logger.create_test_case_logger(config.name)
        run_data.start_time = datetime.now()
        curr_process = multiprocessing.current_process()
        if curr_process and curr_process.ident:
            run_data.process_id = curr_process.pid if curr_process.pid else 0
            run_data.process_name = curr_process.name

        test_result: TestCaseResult = TestCaseResult(config, run_data)

        skip_testcase, skip_postprocessing = self.__check_for_skipping(config)
        if not skip_testcase:
            logger.test_started()
        else:
            logger.test_ignored()

        log_header(
            f"Testcase {run_data.test_number} of {run_data.number_of_tests} "
            + f"(process id {run_data.process_id}): {config.name} ...",
            logger,
        )

        try:
            log_sub_header(f"Preparing test case name = '{config.name}'", logger)
            self.__prepare_test_case(config, logger)
            log_separator(logger, char="-")

            # Run testcase
            testcase = TestCase(config, logger)

            if self.__settings.skip_run:
                logger.info("Skipping execution of testcase (postprocess only)...\n")
            else:
                if not skip_testcase:
                    log_sub_header("Execute testcase...", logger)
                    testcase.run(self.programs)
                    log_separator(logger, char="-")
                else:
                    logger.info("Testcase not executed (ignored)...\n")

            # Check for errors during execution of testcase
            if len(testcase.getErrors()) > 0:
                errstr = ",".join(str(e) for e in testcase.getErrors())
                logger.error("Errors during testcase: " + errstr)
                raise TestCaseFailure("Errors during testcase: " + errstr)

            # Postprocessing
            if not skip_postprocessing:
                log_sub_header("Postprocessing testcase, checking directories...", logger)

                if not os.path.exists(config.absolute_test_case_path):
                    raise TestCaseFailure("Could not locate case data at: " + str(config.absolute_test_case_path))

                # execute concrete method in subclass
                test_result = self.post_process(config, logger, run_data)
                log_separator(logger, char="-")

            if not skip_testcase:
                logger.test_Result(TestResultType.Passed)

        except Exception as exception:
            logger.exception(f"Could not run test case: {repr(exception)}")
            test_result = self.create_error_result(config, run_data)

            if not skip_testcase:
                logger.test_Result(TestResultType.Exception, str(exception))

        logger.test_finished()
        if in_use is not None:
            with idle_process:
                in_use.value -= config.process_count
                idle_process.notify_all()

        return test_result

    @abstractmethod
    def post_process(
        self,
        test_case_config: TestCaseConfig,
        logger: ITestLogger,
        run_data: RunData,
    ) -> TestCaseResult:
        """Post process run results (files)

        Args:
            test_case_config (TestCaseConfig): configuration of the run
            logger (ITestLogger): logger to log to

        Returns:
            TestCaseResult: Result of the post processing
        """
        logger.debug(f"Reference directory:{test_case_config.absolute_test_case_reference_path}")
        logger.debug(f"Results   directory:{test_case_config.absolute_test_case_path}")

    @abstractmethod
    def show_summary(self, results: List[TestCaseResult], logger: ILogger):
        """Shows a summery showing the results of all tests that were run

        Args:
            results (List[TestCaseResult]): list of test results to summarize
            logger (ILogger): logger to log to
        """

    @abstractmethod
    def create_error_result(self, test_case_config: TestCaseConfig, run_data: RunData) -> TestCaseResult:
        """Creates an error result

        Args:
            testCaseConfig (TestCaseConfig): test case to use
            run_data (RunData): Data related to the run

        Returns:
            TestCaseResult: Error result
        """

    def __log_successful_test(self, test_case_result: TestCaseResult):
        self.finished_tests += 1
        run_data = test_case_result.run_data

        max_index_length = len(str(run_data.number_of_tests))

        self.__logger.info(
            f"Finished test ({str(self.finished_tests).rjust(max_index_length)} - {run_data.absolute_duration_str()}) "
            + f"{test_case_result.config.name.ljust(50)}"
            + f"(index: {run_data.index_str()}) "
            + f"{run_data.timing_str()} -> process {run_data.process_id_str()}"
        )

    def __log_failed_test(self, exception: BaseException):
        self.finished_tests += 1
        self.__logger.exception(
            f"Error running ({self.finished_tests}/{len(self.__settings.configs_to_run)}): {repr(exception)}"
        )

    def __check_for_skipping(self, config: TestCaseConfig):
        skip_testcase = False  # No check defined still running (so no regression test, test against measurements or other numerical package)
        skip_postprocessing = True  # No check defined still running and do not perform the standard postprocessing

        if len(config.checks) > 0:
            skip_testcase = True
            skip_postprocessing = True

        for file_check in config.checks:
            if not file_check.ignore:
                skip_testcase = False
                skip_postprocessing = False

        if not skip_testcase:
            if config.ignore:
                skip_testcase = True
                skip_postprocessing = True

        return skip_testcase, skip_postprocessing

    def __download_dependencies(self):
        configs_to_handle = [c for c in self.__settings.configs_to_run if c.dependency]
        if len(configs_to_handle) == 0:
            return

        log_sub_header("Downloading test dependencies", self.__logger)

        for config in configs_to_handle:
            self.__download_config_dependencies(config, self.__logger)

        log_separator(self.__logger, char="-", with_new_line=True)

    def __update_programs(self) -> Iterable[Program]:
        """Update network programs and initialize the stack"""

        log_sub_header("Updating programs", self.__logger)

        for program_configuration in self.__settings.programs:
            self.__logger.info(f"Updating program: {program_configuration.name}")

            # Local path to program root folder
            program_local_path = None

            # Get the program location
            if len(program_configuration.locations) > 0:
                for loc in program_configuration.locations:
                    # check type of program
                    if (self.__settings.run_mode == ModeType.REFERENCE and loc.type == PathType.CHECK) or (
                        self.__settings.run_mode == ModeType.COMPARE and loc.type == PathType.CHECK
                    ):
                        # if the program is local, use the existing location
                        sourceLocation = Paths().mergeFullPath(loc.root, loc.from_path)
                        if Paths().isPath(sourceLocation):
                            absLocation = os.path.abspath(
                                Paths().mergeFullPath(sourceLocation, program_configuration.path)
                            )
                            if ResolveHandler.detect(absLocation, self.__logger, None) == HandlerType.PATH:
                                if not os.path.exists(absLocation):
                                    self.__logger.warning(f"could not yet detect specified program {absLocation}")
                                #                                   raise SystemExit("Program does not exist")
                                else:
                                    self.__logger.debug(
                                        f"detected local path for program {program_configuration.name}, using {absLocation}"
                                    )
                                program_configuration.absolute_bin_path = absLocation
                        # else download it from a remote location
                        else:
                            if loc.version:
                                to = loc.to_path + "_" + loc.version
                            else:
                                to = loc.to_path
                            program_local_path = Paths().rebuildToLocalPath(
                                os.path.join(self.__settings.local_paths.engines_path, to)
                            )

                            # if the program is remote (network or other) and it does not exist locally, download it
                            if not os.path.exists(program_local_path):
                                self.__logger.debug(
                                    f"Downloading program, {program_configuration.name} from {sourceLocation}"
                                )
                                HandlerFactory.download(
                                    sourceLocation,
                                    program_local_path,
                                    self.programs,
                                    self.__logger,
                                    loc.credentials,
                                    loc.version,
                                )
                            program_configuration.absolute_bin_path = os.path.abspath(
                                Paths().mergeFullPath(program_local_path, program_configuration.path)
                            )

            # If a program does not have a network path, and path is not a relative or absolute path, we assume the system can find it
            elif not Paths().isPath(program_configuration.path):
                program_configuration.absolute_bin_path = program_configuration.path
            # Otherwise we need to construct the path from the given information
            else:
                # Construct the absolute binary path for the program
                absbinpath = os.path.abspath(Paths().rebuildToLocalPath(program_configuration.path))
                if os.path.exists(absbinpath):
                    program_configuration.absolute_bin_path = absbinpath
                # If the local program does not exist, and a network path is not given we are going to crash
                else:
                    raise SystemExit(
                        "Could not find " + program_configuration.name + " at given location " + absbinpath
                    )
            self.__logger.debug(
                f"Binary path for program {program_configuration.name}: {program_configuration.absolute_bin_path}"
            )

            # Rebuild the environment variables (specified for this program) to local system variables
            # This is the only place containing all relevant information
            # Do not rebuild the environment variable when it contains a keyword surrounded by "[" and "]",
            # they will be replaced later on
            envparams = program_configuration.environment_variables
            for envparam in envparams:
                if envparams[envparam][0] == "path" and str(envparams[envparam][1]).find("[") == -1:
                    pp = Paths().rebuildToLocalPath(envparams[envparam][1])
                    if not Paths().isAbsolute(pp):
                        if program_local_path:
                            pp = os.path.abspath(Paths().mergeFullPath(program_local_path, pp))
                        envparams[envparam] = [envparams[envparam][0], pp]
                    else:
                        envparams[envparam] = [
                            envparams[envparam][0],
                            envparams[envparam][1],
                        ]

            # Add search paths to the program(configure)
            # Search for (the last) win/lnx/linux in AbsoluteBinPath,
            # add all subdirectories from this level downwards to searchPaths
            # It's quite crude, but this way, all Delft3D programs are able to find each other.
            if program_configuration.add_search_paths:
                pltIndex = max(
                    program_configuration.absolute_bin_path.rfind("win"),
                    program_configuration.absolute_bin_path.rfind("lnx"),
                    program_configuration.absolute_bin_path.rfind("linux"),
                    program_configuration.absolute_bin_path.rfind("x64"),
                )
                if pltIndex > -1:
                    separatorIndex = max(
                        program_configuration.absolute_bin_path[pltIndex:].find("\\"),
                        program_configuration.absolute_bin_path[pltIndex:].find("/"),
                    )
                    pltPath = program_configuration.absolute_bin_path[: pltIndex + separatorIndex]
                    self.__logger.debug("Path: " + pltPath)
                    searchPaths = Paths().findAllSubFolders(
                        pltPath, program_configuration.exclude_search_paths_containing
                    )
                else:
                    # No win/lnx/linux found in AbsoluteBinPath:
                    # Just add AbsoluteBinPath and its subFolders
                    searchPaths = Paths().findAllSubFolders(
                        program_configuration.absolute_bin_path,
                        program_configuration.exclude_search_paths_containing,
                    )
                # Add explicitly named searchPaths, rebuild when needed
                for aPath in program_configuration.search_paths:
                    aRebuildPath = Paths().rebuildToLocalPath(aPath)
                    if not Paths().isAbsolute(aRebuildPath) and program_local_path:
                        aRebuildPath = Paths().mergeFullPath(program_local_path, aRebuildPath)
                    searchPaths.append(aRebuildPath)
                program_configuration.search_paths = searchPaths

            # Initialize the program
            yield Program(program_configuration, self.settings)
        log_separator(self.__logger, char="-", with_new_line=True)

    def __prepare_test_case(self, config: TestCaseConfig, logger: ILogger) -> None:
        """Prepare test case based on provided config
        (download input & reference data)

        Args:
            config (TestCaseConfig): test configuration to prepare

        Raises:
             TestBenchError : if test can not be prepared
        """
        if self.__settings.local_paths is None:
            raise TestBenchError("Local paths are missing from the testbench settings")
        if not config.locations:
            raise TestBenchError(f"Could not update case {config.name}, no network paths given")
        if config.path is None:
            raise TestBenchError(f"Could not update case {config.name}, path is missing")

        logger.info(f"Preparing case: {config.name}")
        if config.path.version is None:
            logger.warning("The case path version timestamp is missing, downloading the 'latest' version")
        else:
            logger.info(f"Path version timestamp: {config.path.version}")

        for location in config.locations:
            if not location.root or not location.from_path:
                error_message: str = (
                    f"Could not prepare case {config.name}"
                    + f", invalid network input path part (root:{location.root},"
                    + f" from:{location.from_path}) given"
                )
                raise TestBenchError(error_message)

            # Build the path to download from: Root+From+testcasePath:
            remote_path = Paths().mergeFullPath(location.root, location.from_path, config.path.prefix)

            if Paths().isPath(remote_path):
                remote_path = os.path.abspath(remote_path)

            # Downloading the testcase input/refdata may fail. Safe side: try 3 times.
            success = False
            attempts = 0

            while attempts < 3 and not success:
                attempts += 1

                try:
                    destination_dir = None

                    if location.type == PathType.INPUT:
                        destination_dir = self.__settings.local_paths.cases_path
                    elif location.type == PathType.REFERENCE:
                        destination_dir = self.__settings.local_paths.reference_path

                    if destination_dir is not None:
                        # Build localPath to download to: To+testcasePath
                        local_path = Paths().rebuildToLocalPath(
                            Paths().mergeFullPath(destination_dir, location.to_path, config.name)
                        )

                        self.__download_files(
                            location, remote_path, local_path, location.type, config.path.version, logger
                        )

                        if location.type == PathType.INPUT:
                            config.absolute_test_case_path = local_path
                        elif location.type == PathType.REFERENCE:
                            config.absolute_test_case_reference_path = local_path

                    success = True

                except Exception as e:
                    error_message = f"Unable to download testcase (attempt {attempts + 1})"

                    if attempts < 3:
                        logger.warning(error_message)
                    else:
                        error = getattr(e, "message", repr(e))
                        error_message = f"Unable to download testcase: {error}"
                        raise TestBenchError(error_message) from e

    def __download_files(
        self,
        location: Location,
        remote_path: str,
        local_path: str,
        location_type: PathType,
        version: Optional[str],
        logger: ILogger,
    ) -> None:
        version = location.version or version
        if location_type == PathType.INPUT:
            location_description = "input of case"
        elif location_type == PathType.REFERENCE:
            location_description = "reference result"
        elif location_type == PathType.DEPENDENCY:
            location_description = "dependency"

        if location_type in self.skip_download:
            logger.info(f"Skipping {location_description} download (skip download argument)")
            return
        else:
            logger.debug(f"Downloading {location_description}, {local_path} from {remote_path}")

        # Download location on local system is always cleaned before start
        try:
            HandlerFactory.download(
                remote_path,
                local_path,
                self.programs,
                logger,
                location.credentials,
                version,
            )
        except Exception as exception:
            # We need always case input data
            logger.exception(f"Could not download from {remote_path}")
            raise exception

    def __download_config_dependencies(self, config: TestCaseConfig, logger: ILogger) -> None:
        if not config.dependency:
            return

        if self.__settings.local_paths is None:
            logger.error("Could not download dependency: Local paths are missing from the testbench settings")
            return

        location = next(loc for loc in config.locations if loc.type == PathType.INPUT)
        destination_dir = self.__settings.local_paths.cases_path

        local_path = Paths().rebuildToLocalPath(
            Paths().mergeFullPath(
                destination_dir,
                location.to_path,
                config.dependency.local_dir,
            )
        )

        if os.path.exists(local_path):
            logger.info("Dependency directory already exists: Skipping download")
            return

        remote_path = Paths().mergeFullPath(location.root, location.from_path, config.dependency.cases_path)
        dependency_version = config.dependency.version
        if dependency_version is None:
            logger.warning("The dependency version timestamp is missing, downloading the 'latest' version")
        else:
            logger.info(f"Dependency version timestamp: {dependency_version}")

        self.__download_files(location, remote_path, local_path, PathType.DEPENDENCY, dependency_version, logger)
