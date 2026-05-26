"""Manager for running test case sets.

Copyright (C)  Stichting Deltares, 2026
"""

import gc
import multiprocessing
import os
import shutil
import sys
from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime, timedelta
from multiprocessing.pool import AsyncResult
from multiprocessing.synchronize import Condition
from pathlib import Path
from typing import Iterable, List, Optional

from src.config.credentials import Credentials
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
from src.utils.handlers.dvc_handler import DvcHandler
from src.utils.handlers.handler_factory import HandlerFactory
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.logging.i_logger import ILogger
from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType
from src.utils.paths import Paths


@dataclass(frozen=True)
class DvcLocationInfo:
    """Resolved paths for a single DVC location within a test case."""

    config: TestCaseConfig
    location: Location
    remote_path: str
    local_path: str


class TestSetRunner(ABC):
    """Run test cases in reference or compare mode."""

    def __init__(self, settings: TestBenchSettings, logger: IMainLogger) -> None:
        self.__settings = settings
        self.__logger = logger
        self.__duration = None
        self.programs: List[Program] = []
        self.skip_download = settings.command_line_settings.skip_download
        self.finished_tests: int = 0

    @property
    def settings(self) -> TestBenchSettings:
        """Settings used for running tests.

        Returns
        -------
        TestBenchSettings
            Used test settings.
        """
        return self.__settings

    @property
    def duration(self) -> Optional[timedelta]:
        """Time it took to run the testbench.

        Returns
        -------
        Optional[timedelta]
            Elapsed time.
        """
        return self.__duration

    def run(self) -> None:
        """Run test cases to generate reference data."""
        start_time = datetime.now()

        if len(self.settings.configs_to_run) == 0:
            logline = (
                f"There are no test cases in '{self.settings.command_line_settings.config_file}' "
                f"with applied filter '{self.settings.command_line_settings.filter}'."
            )
            self.__logger.error(logline)
            raise ValueError(logline)

        try:
            self.programs = list(self.__update_programs())
        except Exception:
            if self.__settings.command_line_settings.teamcity:
                sys.stderr.write("##teamcity[testStarted name='Update programs']\n")
                sys.stderr.write("##teamcity[testFailed name='Update programs' message='Exception occurred']\n")

        self.__download_dependencies()
        log_sub_header("Running tests", self.__logger)

        # Prepare DVC cases: batch-download all .dvc files in one command
        self.__prepare_dvc_test_cases()

        # Free memory accumulated during DVC preparation (repo objects, file
        # indices, etc.) so the forked worker pool starts with a lean parent.
        gc.collect()

        results = (
            self.run_tests_in_parallel()
            if self.__settings.command_line_settings.parallel
            else self.run_tests_sequentially()
        )

        log_separator(self.__logger, char="-", with_new_line=True)

        if results:
            if not self.__settings.command_line_settings.skip_post_processing:
                self.show_summary(results, self.__logger)
                if self.settings.command_line_settings.copy_failed_cases:
                    self._copy_failed_cases(results, Path("test/deltares_testbench/copy_cases"))
            else:
                self.__logger.info("No summary, because postprocessing is skipped due to argument.")
        else:
            raise ValueError("ERROR: There are no results, which is unexpected.")

        self.__duration = datetime.now() - start_time

    def run_tests_sequentially(self) -> List[TestCaseResult]:
        """Run the test configurations sequentially and returns the results.

        Returns
        -------
        List[TestCaseResult]
            List of test results.
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
        """Run the test configurations in parallel and returns the results.

        Returns
        -------
        List[TestCaseResult]
            List of test results.
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
        self,
        config: TestCaseConfig,
        run_data: RunData,
        in_use: Optional[int] = None,
        idle_process: Optional[Condition] = None,
    ) -> TestCaseResult:
        """Run one test configuration (in a separate process).

        Parameters
        ----------
        config : TestCaseConfig
            Configuration to run.
        run_data : RunData
            Data related to the test run.
        in_use : Optional[int], default: None
            Amount of processes that are currently in use with testcases.
        idle_process : Optional[Condition], default: None
            Sends a notification to evaluate available cores for new testcase.
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
            if not config.path or config.path.version != "DVC":
                log_sub_header(f"Preparing test case name = '{config.name}'", logger)
                self.prepare_test_case(config, logger)
                log_separator(logger, char="-")
            else:
                # DVC data is batch-downloaded upfront; create a fresh work copy for this run.
                if config.absolute_test_case_path:
                    work_path = Path(config.absolute_test_case_path)
                    if work_path.name.endswith("_work") and not work_path.exists():
                        source_path = work_path.with_name(work_path.name[:-5])
                        self.__copy_to_work_folder(source_path, logger)

            # Run testcase
            if not config.absolute_test_case_path or not config.absolute_test_case_reference_path:
                raise TestBenchError("Test case paths are not prepared.")
            testcase = TestCase(config, logger)

            if self.__settings.command_line_settings.skip_run:
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
        """Post process run results (files).

        Parameters
        ----------
        test_case_config : TestCaseConfig
            Configuration of the run.
        logger : ITestLogger
            Logger to log to.

        Returns
        -------
        TestCaseResult
            Result of the post processing.
        """
        logger.debug(f"Reference directory:{test_case_config.absolute_test_case_reference_path}")
        logger.debug(f"Results   directory:{test_case_config.absolute_test_case_path}")

    @abstractmethod
    def show_summary(self, results: List[TestCaseResult], logger: ILogger):
        """Show a summery showing the results of all tests that were run.

        Parameters
        ----------
        results : List[TestCaseResult]
            List of test results to summarize.
        logger : ILogger
            Logger to log to.
        """

    def _copy_failed_cases(self, results: list[TestCaseResult], dest_path: Path):

        for result in results:
            test_case_config = result.config
            test_case_results = result.results

            if any(not comparison.passed for (_, _, _, comparison) in test_case_results):
                shutil.copytree(test_case_config.absolute_test_case_path, dest_path / test_case_config.name)


    @abstractmethod
    def create_error_result(self, test_case_config: TestCaseConfig, run_data: RunData) -> TestCaseResult:
        """Create an error result.

        Parameters
        ----------
        test_case_config : TestCaseConfig
            Test case to use.
        run_data : (RunData)
            Data related to the run.

        Returns
        -------
        TestCaseResult
            Error result.
        """

    def __log_successful_test(self, test_case_result: TestCaseResult) -> None:
        self.finished_tests += 1
        run_data = test_case_result.run_data

        max_index_length = len(str(run_data.number_of_tests))

        self.__logger.info(
            f"Finished test ({str(self.finished_tests).rjust(max_index_length)} - {run_data.absolute_duration_str()}) "
            + f"{test_case_result.config.name.ljust(50)}"
            + f"(index: {run_data.index_str()}) "
            + f"{run_data.timing_str()} -> process {run_data.process_id_str()}"
        )

    def __log_failed_test(self, exception: BaseException) -> None:
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

        if self.settings.command_line_settings.skip_post_processing:
            skip_postprocessing = True

        return skip_testcase, skip_postprocessing

    def cleanup_failed_preparation(self, config: TestCaseConfig) -> None:
        """Clean up partially downloaded files after preparation failure.

        Parameters
        ----------
        config : TestCaseConfig
            Configuration of the test case that failed to prepare.
        """
        # Clean up input directory (without _work suffix)
        if config.absolute_test_case_path:
            input_path = Path(config.absolute_test_case_path)
            if input_path.name.endswith("_work"):
                original_input = input_path.with_name(input_path.name[:-5])  # Remove "_work"
                if original_input.exists():
                    self.__logger.debug(f"Cleaning up input directory: {original_input}")
                    try:
                        shutil.rmtree(original_input)
                    except Exception as e:
                        self.__logger.warning(f"Failed to remove input directory: {e}")

        # Clean up reference directory if it was created
        if config.absolute_test_case_reference_path and os.path.exists(config.absolute_test_case_reference_path):
            self.__logger.debug(f"Cleaning up reference directory: {config.absolute_test_case_reference_path}")
            try:
                shutil.rmtree(config.absolute_test_case_reference_path)
            except Exception as e:
                self.__logger.warning(f"Failed to remove reference directory: {e}")

    def __prepare_dvc_test_cases(self) -> None:
        """Prepare all DVC test cases with a single batched download."""
        dvc_configs = [c for c in self.__settings.configs_to_run if c.path and c.path.version == "DVC"]
        if not dvc_configs:
            return

        log_sub_header("Preparing DVC test cases (batch download)", self.__logger)

        all_dvc_locations = self.__collect_dvc_locations(dvc_configs)

        dvc_files = [
            dvc_location.remote_path
            for dvc_location in all_dvc_locations
            if dvc_location.location.type not in self.skip_download
        ]

        # Include dependency .dvc files in the batch download
        dependency_dvc_files = self.__collect_dependency_dvc_files(dvc_configs)
        dvc_files.extend(dependency_dvc_files)

        if not self.__batch_download_dvc(dvc_files, all_dvc_locations, dvc_configs):
            return

        self.__apply_dvc_paths(all_dvc_locations)

        # Copy DVC dependencies now that the batch checkout has made the data available.
        self.__copy_dvc_dependencies(dvc_configs)

        self.__create_dvc_work_copies(dvc_configs)

        log_separator(self.__logger, char="-")

    def __create_dvc_work_copies(self, dvc_configs: list[TestCaseConfig]) -> None:
        """Create _work copies of all DVC input directories serially."""
        for config in dvc_configs:
            if not config.absolute_test_case_path:
                continue
            work_path = Path(config.absolute_test_case_path)
            if work_path.name.endswith("_work"):
                source_path = work_path.with_name(work_path.name[:-5])
                if source_path.is_dir():
                    self.__copy_to_work_folder(source_path, self.__logger)

    def __copy_dvc_dependencies(self, dvc_configs: list[TestCaseConfig]) -> None:
        """Copy DVC dependency data to the expected location next to each test case's input."""
        configs_with_deps = [c for c in dvc_configs if c.dependency and c.dependency.version == "DVC"]
        if not configs_with_deps:
            return

        log_sub_header("Copying DVC dependencies", self.__logger)
        for config in configs_with_deps:
            self.__download_config_dependencies(config, self.__logger)

    def __collect_dvc_locations(self, dvc_configs: list[TestCaseConfig]) -> list[DvcLocationInfo]:
        """Validate each DVC config and collect its downloadable locations."""
        all_dvc_locations: list[DvcLocationInfo] = []

        for config in dvc_configs:
            try:
                self.__validate_test_case_preparation(config)
                self.__log_preparation_info(config, self.__logger)
            except Exception as exception:
                self.__logger.error(f"Failed to validate test case '{config.name}': {exception}")
                self.cleanup_failed_preparation(config)
                continue

            locations = self.__collect_locations_for_config(config)
            if locations is not None:
                all_dvc_locations.extend(locations)

        return all_dvc_locations

    def __collect_locations_for_config(self, config: TestCaseConfig) -> list[DvcLocationInfo] | None:
        """Collect location info for a single config. Returns None on validation failure."""
        infos: list[DvcLocationInfo] = []
        for location in config.locations:
            if location.type == PathType.CHECK:
                continue
            try:
                self.__validate_location(config, location)
            except Exception as exception:
                self.__logger.error(f"Failed to validate location for '{config.name}': {exception}")
                self.cleanup_failed_preparation(config)
                return None

            infos.append(
                DvcLocationInfo(
                    config=config,
                    location=location,
                    remote_path=self.__build_remote_path(config, location),
                    local_path=self.__build_local_path(config, location),
                )
            )
        return infos

    def __collect_dependency_dvc_files(self, dvc_configs: list[TestCaseConfig]) -> list[str]:
        """Collect .dvc file paths for DVC dependencies so they are included in the batch download."""
        seen: set[str] = set()
        dvc_files: list[str] = []
        for config in dvc_configs:
            if not config.dependency or config.dependency.version != "DVC":
                continue
            location = next((loc for loc in config.locations if loc.type == PathType.INPUT), None)
            if location is None:
                continue
            dep_dvc_path = os.path.abspath(
                Paths().rebuildToLocalPath(Paths().mergeFullPath(location.root, config.dependency.cases_path + ".dvc"))
            )
            if dep_dvc_path not in seen and os.path.isfile(dep_dvc_path):
                seen.add(dep_dvc_path)
                dvc_files.append(dep_dvc_path)
                self.__logger.debug(f"Including dependency DVC file: {dep_dvc_path}")
        return dvc_files

    def __batch_download_dvc(
        self,
        dvc_files: list[str],
        location_infos: list[DvcLocationInfo],
        dvc_configs: list[TestCaseConfig],
    ) -> bool:
        """Batch-download all .dvc files. Returns False if download failed."""
        if not dvc_files:
            return True

        credentials = self.__find_dvc_credentials(location_infos)
        try:
            handler = DvcHandler()
            handler.download_batch(
                dvc_files,
                credentials,
                self.__logger,
            )
            return True
        except Exception as exception:
            self.__logger.error(f"Batch DVC download failed: {exception}")
            for config in dvc_configs:
                self.cleanup_failed_preparation(config)
            log_separator(self.__logger, char="-")
            return False

    @staticmethod
    def __find_dvc_credentials(location_infos: list[DvcLocationInfo]) -> Credentials:
        """Return the first credentials found in the location list, or empty defaults."""
        for info in location_infos:
            if info.location.credentials and info.location.credentials.username:
                return info.location.credentials
        return Credentials()

    def __apply_dvc_paths(self, location_infos: list[DvcLocationInfo]) -> None:
        """Set absolute paths on configs after a successful batch download."""
        for info in location_infos:
            try:
                self.__set_absolute_paths(info.config, info.location.type, info.local_path)
            except Exception as exception:
                self.__logger.error(f"Failed post-download steps for '{info.config.name}': {exception}")
                self.cleanup_failed_preparation(info.config)

    def __download_dependencies(self) -> None:
        # DVC dependencies are handled after the DVC batch checkout in __prepare_dvc_test_cases.
        configs_to_handle = [
            c for c in self.__settings.configs_to_run if c.dependency and not (c.dependency.version == "DVC")
        ]
        if len(configs_to_handle) == 0:
            return

        log_sub_header("Downloading test dependencies", self.__logger)

        for config in configs_to_handle:
            self.__download_config_dependencies(config, self.__logger)

        log_separator(self.__logger, char="-", with_new_line=True)

    def __update_programs(self) -> Iterable[Program]:
        """Update network programs and initialize the stack."""
        log_sub_header("Updating programs", self.__logger)

        for program_configuration in self.__settings.programs:
            self.__logger.info(f"Updating program: {program_configuration.name}")

            # Local path to program root folder
            program_local_path = None

            # Get the program location
            if len(program_configuration.locations) > 0:
                for loc in program_configuration.locations:
                    # check type of program
                    if (
                        self.__settings.command_line_settings.run_mode == ModeType.REFERENCE
                        and loc.type == PathType.CHECK
                    ) or (
                        self.__settings.command_line_settings.run_mode == ModeType.COMPARE
                        and loc.type == PathType.CHECK
                    ):
                        # if the program is local, use the existing location
                        # Try loc.root first, then fall back to engines_path
                        sourceLocation = Paths().mergeFullPath(loc.root, loc.from_path)
                        enginesLocation = Paths().mergeFullPath(self.__settings.local_paths.engines_path, loc.from_path)
                        resolved = False
                        for candidate_source in [sourceLocation, enginesLocation]:
                            if not Paths().isPath(candidate_source):
                                continue
                            absLocation = os.path.abspath(
                                Paths().mergeFullPath(candidate_source, program_configuration.path)
                            )
                            if ResolveHandler.detect(absLocation, self.__logger, None) == HandlerType.PATH:
                                if os.path.exists(absLocation):
                                    self.__logger.debug(
                                        f"detected local path for program {program_configuration.name}, using {absLocation}"
                                    )
                                    program_configuration.absolute_bin_path = absLocation
                                    resolved = True
                                    break
                        if not resolved:
                            if Paths().isPath(sourceLocation):
                                # Use engines_path as default when not found locally
                                absLocation = os.path.abspath(
                                    Paths().mergeFullPath(enginesLocation, program_configuration.path)
                                )
                                self.__logger.warning(f"could not yet detect specified program {absLocation}")
                                program_configuration.absolute_bin_path = absLocation
                            else:
                                # download it from a remote location
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

    def prepare_test_case(self, config: TestCaseConfig, logger: ILogger) -> None:
        """Prepare test case based on provided config (download input & reference data).

        Parameters
        ----------
        config : TestCaseConfig
            Test configuration to prepare.

        Raises
        ------
        TestBenchError
            If test can not be prepared.
        """
        self.__validate_test_case_preparation(config)
        self.__log_preparation_info(config, logger)
        self.__process_test_case_locations(config, logger)

    def __validate_test_case_preparation(self, config: TestCaseConfig) -> None:
        """Validate that test case has all required configuration for preparation."""
        if self.__settings.local_paths is None:
            raise TestBenchError("Local paths are missing from the testbench settings")
        if not config.locations:
            raise TestBenchError(f"Could not update case {config.name}, no network paths given")

    def __log_preparation_info(self, config: TestCaseConfig, logger: ILogger) -> None:
        """Log information about test case preparation."""
        logger.info(f"Preparing case: {config.name}")
        if config.path and config.path.version is None:
            logger.warning("The case path version timestamp is missing, downloading the 'latest' version")
        elif config.path and config.path.version:
            logger.info(f"Path version timestamp: {config.path.version}")

    def __process_test_case_locations(self, config: TestCaseConfig, logger: ILogger) -> None:
        """Process all locations for a test case, downloading files as needed."""
        for location in config.locations:
            # Skip CHECK locations — those are for program binaries, handled by __update_programs
            if location.type == PathType.CHECK:
                continue
            self.__validate_location(config, location)
            remote_path = self.__build_remote_path(config, location)
            local_path = self.__build_local_path(config, location)
            self.__download_location_with_retries(config, location, remote_path, local_path, logger)
            if location.type == PathType.INPUT:
                self.__copy_to_work_folder(Path(local_path), logger)

            self.__set_absolute_paths(config, location.type, local_path)

    def __validate_location(self, config: TestCaseConfig, location: Location) -> None:
        """Validate that a location has required configuration."""
        if not location.root or not location.from_path:
            error_message = (
                f"Could not prepare case {config.name}"
                f", invalid network input path part (root:{location.root},"
                f" from:{location.from_path}) given"
            )
            raise TestBenchError(error_message)

    def __build_remote_path(self, config: TestCaseConfig, location: Location) -> str:
        """Build the remote path to download from."""
        if config.path.version == "DVC":
            remote_path = Paths().mergeFullPath(location.root, config.path.prefix)
            if location.type == PathType.INPUT:
                remote_path = Paths().mergeFullPath(remote_path, "input.dvc")
            elif location.type == PathType.REFERENCE and location.from_path != "":
                remote_path = Paths().mergeFullPath(remote_path, f"reference_{location.from_path}.dvc")
            else:
                error_message = (
                    f"Could not build remote path for {config.name}"
                    f", only input and reference (with OS spec) paths are supported for DVC downloads."
                )
                raise TestBenchError(error_message)
        elif config.path:
            remote_path = Paths().mergeFullPath(location.root, location.from_path, config.path.prefix)
        else:
            # For input_path/reference_path cases, use location paths directly
            remote_path = Paths().mergeFullPath(location.root, location.from_path)

        if Paths().isPath(remote_path):
            remote_path = os.path.abspath(remote_path)

        return remote_path

    def __build_local_path(self, config: TestCaseConfig, location: Location) -> str:
        """Build the local path to download to."""
        base_path = self.__get_destination_directory(location.type)
        if config.path.version == "DVC":
            local_path = Paths().mergeFullPath(location.root, config.path.prefix)
            if location.type == PathType.INPUT:
                local_path = Paths().mergeFullPath(local_path, "input")
            elif location.type == PathType.REFERENCE and location.from_path != "":
                local_path = Paths().mergeFullPath(local_path, f"reference_{location.from_path}")
            else:
                error_message = (
                    f"Could not build local path for {config.name}"
                    f", only input and reference (with OS spec) paths are supported for DVC downloads."
                )
                raise TestBenchError(error_message)
        else:
            local_path = Paths().rebuildToLocalPath(Paths().mergeFullPath(base_path, location.to_path, config.name))

        return local_path

    def __download_location_with_retries(
        self, config: TestCaseConfig, location: Location, remote_path: str, local_path: str, logger: ILogger
    ) -> None:
        """Download files for a location with retry logic."""
        attempts = 0
        max_attempts = 3

        for _ in range(max_attempts):
            try:
                self.__download_single_location(config, location, remote_path, local_path, logger)
                break
            except Exception as e:
                error_message = f"Unable to download testcase (attempt {attempts})"

                if attempts < max_attempts:
                    logger.warning(error_message)
                else:
                    error = getattr(e, "message", repr(e))
                    error_message = f"Unable to download testcase: {error}"
                    raise TestBenchError(error_message) from e

    def __copy_to_work_folder(self, local_path: Path, logger: ILogger) -> None:
        """Copy downloaded files to work folder if needed."""
        if not local_path.is_dir():
            raise NotADirectoryError(f"Expected a directory to copy to work folder, but got: {local_path}")

        # Add "_work" suffix.
        copy_path = local_path.with_name(f"{local_path.name}_work")

        # Clean work directory if it exists
        if copy_path.exists():
            shutil.rmtree(copy_path)

        # copy input to work directory
        logger.debug(f"Copying input from {local_path} to {copy_path}")
        shutil.copytree(local_path, copy_path, symlinks=False, ignore_dangling_symlinks=True)

    def __download_single_location(
        self, config: TestCaseConfig, location: Location, remote_path: str, local_path: str, logger: ILogger
    ) -> None:
        """Download files for a single location attempt."""
        version = config.path.version if config.path else None
        self.__download_files(location, remote_path, local_path, location.type, version, logger)

    def __get_destination_directory(self, location_type: PathType) -> Optional[str]:
        """Get the destination directory based on location type."""
        if location_type == PathType.INPUT:
            return self.__settings.local_paths.cases_path
        elif location_type == PathType.REFERENCE:
            return self.__settings.local_paths.reference_path
        return None

    def __set_absolute_paths(self, config: TestCaseConfig, location_type: PathType, local_path: str) -> None:
        """Set absolute paths on the config based on location type."""
        if location_type == PathType.INPUT:
            input_path = Path(local_path)
            config.absolute_test_case_path = str(input_path.with_name(f"{input_path.name}_work"))
        elif location_type == PathType.REFERENCE:
            config.absolute_test_case_reference_path = local_path

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
        elif version == "DVC":
            logger.debug(f"Downloading {location_description}, from DVC file at {remote_path}")
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
        dependency_version = config.dependency.version

        logger.debug(
            f"Dependency config - version: {dependency_version}, test path version: {config.path.version if config.path else None}"
        )
        logger.debug(f"Dependency local_dir: {config.dependency.local_dir}, cases_path: {config.dependency.cases_path}")

        destination_dir = self.__settings.local_paths.cases_path

        if dependency_version == "DVC":
            # DVC dependencies are already checked out at {location.root}/{cases_path}/.
            # Place the dependency as a sibling of the test case's input directory,
            # so that relative paths like ../local_dir resolve correctly from input_work.
            local_path = Paths().rebuildToLocalPath(
                Paths().mergeFullPath(location.root, config.path.prefix, config.dependency.local_dir)
            )
            if os.path.isdir(local_path) and any(os.scandir(local_path)):
                logger.info("Dependency directory already exists: Skipping download")
                return

            source_input_path = Path(
                Paths().rebuildToLocalPath(Paths().mergeFullPath(location.root, config.dependency.cases_path, "input"))
            )
            if source_input_path.is_dir():
                logger.info(f"Copying DVC dependency from {source_input_path} to {local_path}")
                shutil.copytree(str(source_input_path), local_path, dirs_exist_ok=True)
            else:
                logger.error(
                    f"DVC dependency source not found at {source_input_path}. "
                    "Ensure the dependency's .dvc files have been checked out."
                )
            return

        local_path = Paths().rebuildToLocalPath(
            Paths().mergeFullPath(
                destination_dir,
                location.to_path,
                config.dependency.local_dir,
            )
        )

        if os.path.isdir(local_path) and any(os.scandir(local_path)):
            logger.info("Dependency directory already exists: Skipping download")
            return

        remote_path = Paths().mergeFullPath(location.root, location.from_path, config.dependency.cases_path)

        if dependency_version is None:
            logger.warning("The dependency version timestamp is missing, downloading the 'latest' version")
        else:
            logger.info(f"Dependency version timestamp: {dependency_version}")

        self.__download_files(location, remote_path, local_path, PathType.DEPENDENCY, dependency_version, logger)
