import glob
import os
import pathlib as pl
from datetime import datetime, timezone
from enum import Enum
from typing import List
from unittest.mock import MagicMock, PropertyMock, call

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture

from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.path_type import PathType
from src.suite.comparison_runner import ComparisonRunner
from src.suite.run_data import RunData
from src.suite.test_bench_settings import TestBenchSettings
from src.suite.test_case_result import TestCaseResult
from src.utils.common import get_default_logging_folder_path
from src.utils.comparers.end_result import EndResult
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.paths import Paths
from src.utils.xml_config_parser import XmlConfigParser


class FakeDownloadMode(Enum):
    ALL = "all"
    REFS_ONLY = "refs_only"
    FILES = "files"
    OVERWRITE = "overwrite"


def patch_fake_download(mocker: MockerFixture, fs: FakeFilesystem, mode: FakeDownloadMode) -> MagicMock:
    def _fake_download(
        from_path: str,
        to_path: str,
        programs,
        logger,
        credentials,
        version,
    ) -> None:
        match mode:
            case FakeDownloadMode.ALL:
                fs.makedirs(to_path, exist_ok=True)
                return
            case FakeDownloadMode.REFS_ONLY:
                if to_path.startswith("/refs"):
                    fs.makedirs(to_path, exist_ok=True)
                return
            case FakeDownloadMode.FILES:
                if to_path.startswith("/refs"):
                    fs.makedirs(to_path, exist_ok=True)
                elif to_path.startswith("/cases"):
                    fs.makedirs(to_path, exist_ok=True)
                    fs.makedirs(f"{to_path}/sub", exist_ok=True)
                    fs.create_file(f"{to_path}/sub/real.txt", contents="hello")
            case FakeDownloadMode.OVERWRITE:
                if to_path.startswith("/refs"):
                    fs.makedirs(to_path, exist_ok=True)
                elif to_path.startswith("/cases"):
                    fs.makedirs(to_path, exist_ok=True)
                    fs.create_file(f"{to_path}/file.txt", contents="new")

    return mocker.patch("src.suite.test_set_runner.HandlerFactory.download", side_effect=_fake_download)


class TestComparisonRunner:
    @pytest.mark.usefixtures("fs")  # Use fake filesystem.
    def test_run_tests_and_debug_log_downloaded_file(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.parallel = False

        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config("Name_1", locations=[ref_location, case_location])
        config.path = TestCasePath("abc/prefix", "v1")
        settings.configs_to_run = [config]

        logger = MagicMock(spec=ConsoleLogger)
        test_case_logger = MagicMock()
        logger.create_test_case_logger.return_value = test_case_logger

        download_mock = patch_fake_download(mocker, fs, FakeDownloadMode.ALL)

        runner = ComparisonRunner(settings, logger)
        mocker.patch.object(runner, "_TestSetRunner__update_programs", return_value=[])
        mocker.patch.object(runner, "_TestSetRunner__download_dependencies")
        mocker.patch.object(runner, "show_summary", return_value=None)

        # Act
        runner.run()

        # Assert
        ref_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("references", "win64", "Name_1"))
        case_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("cases", "win64", "Name_1"))
        ref_remote = "https://deltares.nl/win64/abc/prefix"
        case_remote = "https://deltares.nl/win64/abc/prefix"

        expected_ref_log = f"Downloading reference result, {ref_path} from {ref_remote}"
        expected_case_log = f"Downloading input of case, {case_path} from {case_remote}"

        assert call(expected_ref_log) in test_case_logger.debug.call_args_list
        assert call(expected_case_log) in test_case_logger.debug.call_args_list

        assert download_mock.call_count == 2
        download_mock.assert_has_calls(
            [
                call(ref_remote, ref_path, runner.programs, test_case_logger, ref_location.credentials, "v1"),
                call(case_remote, case_path, runner.programs, test_case_logger, case_location.credentials, "v1"),
            ],
            any_order=True,
        )

    def test_log_and_skip_with_argument_skip_run(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config("Name_1", locations=[ref_location, case_location])
        config.path = TestCasePath("abc/prefix", "vl")
        config.absolute_test_case_path = "/fake/case/path"
        config.absolute_test_case_reference_path = "/fake/reference/path"
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        run_mock = mocker.patch("src.suite.test_case.TestCase.run")
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        expected_log_message = "Skipping execution of testcase (postprocess only)...\n"
        assert call(expected_log_message) in testcase_logger.info.call_args_list
        run_mock.assert_not_called()

    def test_prepare_case_uses_minio(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
        version = now.isoformat().split("+", 1)[0]
        testcase_path = TestCasePath(prefix="abc/prefix", version=version)
        config = TestComparisonRunner.create_test_case_config(
            name="testname", testcase_path=testcase_path, locations=[ref_location, case_location]
        )
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        mocker.patch.object(ComparisonRunner, "show_summary")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run()

        # Assert
        ref_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("references", "win64", "testname"))
        case_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("cases", "win64", "testname"))
        expected_log_message1 = f"Downloading reference result, {ref_path} from https://deltares.nl/win64/abc/prefix"
        expected_log_message2 = f"Downloading input of case, {case_path} from https://deltares.nl/win64/abc/prefix"
        assert call(expected_log_message1) in testcase_logger.debug.call_args_list
        assert call(expected_log_message2) in testcase_logger.debug.call_args_list

    def test_prepare_case_uses_dvc(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths(cases_path="data/cases", references_path="data/cases")
        settings.command_line_settings.skip_run = True

        testcase_path = TestCasePath(prefix="abc/prefix", version="DVC")

        ref_location = TestComparisonRunner.create_location(
            name="reference", root="data/cases/", location_type=PathType.REFERENCE
        )
        case_location = TestComparisonRunner.create_location(
            name="case", root="data/cases/", location_type=PathType.INPUT
        )
        config = TestComparisonRunner.create_test_case_config(
            name="testname", testcase_path=testcase_path, locations=[ref_location, case_location]
        )
        config.path = TestCasePath("abc/prefix", "DVC")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        mocker.patch.object(ComparisonRunner, "show_summary")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run()

        # Assert
        remote_ref_path = os.path.abspath("data/cases/abc/prefix/reference_win64.dvc")
        remote_case_path = os.path.abspath("data/cases/abc/prefix/input.dvc")
        expected_log_message1 = f"Downloading reference result, from DVC file at {remote_ref_path}"
        expected_log_message2 = f"Downloading input of case, from DVC file at {remote_case_path}"
        assert call(expected_log_message1) in logger.debug.call_args_list
        assert call(expected_log_message2) in logger.debug.call_args_list

    def test_run_tests_in_parallel_with_empty_settings_raises_value_error(self) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.configs_to_run = []
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act & Assert
        with pytest.raises(ValueError):
            runner.run_tests_in_parallel()

    def test_run_tests_in_parallel_with_ignore_check_if_log_file_exist(self) -> None:
        # Arrange
        log_folder_path = get_default_logging_folder_path()
        log_file_1 = os.path.join(log_folder_path, "Name_1", "Name_1.log")
        log_file_2 = os.path.join(log_folder_path, "Name_2", "Name_2.log")
        TestComparisonRunner.clean_empty_logs(log_file_1)
        TestComparisonRunner.clean_empty_logs(log_file_2)
        settings = TestBenchSettings()
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config1 = TestComparisonRunner.create_test_case_config(
            "Name_1", ignore_testcase=True, locations=[ref_location, case_location]
        )
        config2 = TestComparisonRunner.create_test_case_config("Name_2", locations=[ref_location, case_location])
        settings.configs_to_run = [config1, config2]
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_in_parallel()

        # Assert
        TestComparisonRunner.assert_is_file(log_file_1)
        TestComparisonRunner.assert_is_file(log_file_2)

    def test_run_without_test_cases_logs_no_results(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.config_file = "some.xml"
        settings.local_paths = LocalPaths()
        settings.command_line_settings.parallel = False
        logger = MagicMock(spec=ConsoleLogger)

        runner = ComparisonRunner(settings, logger)

        # Act
        with pytest.raises(ValueError):
            runner.run()

        # Assert
        assert (
            call(
                f"There are no test cases in "
                f"'{settings.command_line_settings.config_file}' with applied filter "
                f"'{settings.command_line_settings.filter}'."
            )
            in logger.error.call_args_list
        )

    def test_run_without_test_cases_due_to_filter_logs_no_results_with_filter_suggestion(
        self, mocker: MockerFixture
    ) -> None:
        # Arrange
        settings = TestBenchSettings()
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config1 = TestComparisonRunner.create_test_case_config(
            "Banana_1", ignore_testcase=True, locations=[ref_location, case_location]
        )
        config2 = TestComparisonRunner.create_test_case_config("Banana_2", locations=[ref_location, case_location])
        settings.command_line_settings.config_file = "some.xml"
        xml_configs = [config1, config2]
        settings.local_paths = LocalPaths()
        settings.command_line_settings.parallel = False
        settings.command_line_settings.filter = "testcase=Apple"
        logger = MagicMock(spec=ConsoleLogger)

        runner = ComparisonRunner(settings, logger)

        # Act
        settings.configs_to_run = XmlConfigParser.filter_configs(
            xml_configs, settings.command_line_settings.filter, logger
        )

        with pytest.raises(ValueError):
            runner.run()

        # Assert
        assert (
            call(
                f"There are no test cases in "
                f"'{settings.command_line_settings.config_file}' with applied filter "
                f"'{settings.command_line_settings.filter}'."
            )
            in logger.error.call_args_list
        )

    def test_run_tests_sequentially__run_multiple__continue_on_error(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_run = False
        settings.command_line_settings.parallel = False
        settings.command_line_settings.skip_post_processing = True
        settings.local_paths = LocalPaths()

        # Create one failing and one succeeding `TestCaseConfig`
        locations = [
            self.create_location(name="reference", location_type=PathType.REFERENCE),
            self.create_location(name="case", location_type=PathType.INPUT),
        ]
        program = ProgramConfig()
        program.name = "frobnicate"
        settings.programs = [program]
        failing_config = self.create_test_case_config("i_am_error", locations=locations)
        failing_config.program_configs = [program]
        succeeding_config = self.create_test_case_config("i_always_succeed", locations=locations)
        succeeding_config.program_configs = [program]

        # Create the `ComparisonRunner`
        settings.configs_to_run = [failing_config, succeeding_config]
        logger = mocker.Mock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)
        runner.programs = list(runner._TestSetRunner__update_programs())  # type: ignore

        # Set up directories using fake download and prepare_test_case
        patch_fake_download(mocker, fs, FakeDownloadMode.ALL)
        runner.prepare_test_case(failing_config, logger)
        runner.prepare_test_case(succeeding_config, logger)

        # Patch program execution behavior
        mocker.patch("src.suite.test_case.Program.run")
        return_code_mock = mocker.patch("src.suite.test_case.Program.last_return_code", new_callable=PropertyMock)
        return_code_mock.side_effect = [1, 0]  # First test fails, second succeeds

        # Make `getError` first return an error, then no error
        return_values = iter([RuntimeError("Failed to frobnicate"), None])
        mocker.patch("src.suite.test_case.Program.getError", side_effect=lambda: next(return_values))

        # Patch post_process to return empty results
        mocker.patch.object(
            ComparisonRunner,
            "post_process",
            side_effect=lambda config, logger, run_data: TestCaseResult(config, run_data),
        )

        # Act
        failed, succeeded, *others = runner.run_tests_sequentially()

        # Assert
        assert not others
        assert failed.results[0][-1].result == EndResult.ERROR
        assert not succeeded.results  # No `EndResult.ERROR` in `results` means the comparison can potentially succeed.

    def test_case_preperation_makes_copy_of_case_dir(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = []
        settings.local_paths = LocalPaths(cases_path="/cases", references_path="/refs")
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config(
            "Banana_1", ignore_testcase=True, locations=[ref_location, case_location]
        )
        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        patch_fake_download(mocker, fs, FakeDownloadMode.ALL)

        expected_work_path = "/cases/win64/Banana_1_work"

        # Act
        runner.prepare_test_case(config=config, logger=logger)

        # Assert
        assert fs.exists(expected_work_path)

    def test_copy_to_work_folder__missing_source__raises_error(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = []
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_post_processing = True
        settings.local_paths = LocalPaths(cases_path="/cases", references_path="/refs")

        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config(
            "Name_1", ignore_testcase=True, locations=[ref_location, case_location]
        )

        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        expected_local_input_path = "/cases/win64/Name_1"
        expected_work_path = expected_local_input_path + "_work"

        patch_fake_download(mocker, fs, FakeDownloadMode.REFS_ONLY)

        # Make the input path exist but not be a directory.
        fs.create_file(expected_local_input_path, contents="not a directory")

        # Act & Assert
        with pytest.raises(NotADirectoryError, match="Expected a directory to copy to work folder"):
            runner.prepare_test_case(config=config, logger=logger)

        assert not fs.exists(expected_work_path)

    def test_copy_to_work_folder__copies_directory(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = []
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_post_processing = True
        settings.local_paths = LocalPaths(cases_path="/cases", references_path="/refs")

        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config(
            "Name_1", ignore_testcase=True, locations=[ref_location, case_location]
        )

        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        expected_local_input_path = "/cases/win64/Name_1"
        expected_work_path = expected_local_input_path + "_work"

        patch_fake_download(mocker, fs, FakeDownloadMode.FILES)

        # Act
        runner.prepare_test_case(config=config, logger=logger)

        # Assert
        assert fs.exists(f"{expected_work_path}/sub/real.txt")

    def test_copy_to_work_folder__overwrites_existing_work_folder(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = []
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_post_processing = True
        settings.local_paths = LocalPaths(cases_path="/cases", references_path="/refs")

        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config(
            "Name_1", ignore_testcase=True, locations=[ref_location, case_location]
        )

        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        runner = ComparisonRunner(settings, logger)
        run_data = RunData(1, 1)

        expected_local_input_path = "/cases/win64/Name_1"
        expected_work_path = expected_local_input_path + "_work"

        fs.makedirs(expected_work_path, exist_ok=True)
        fs.create_file(f"{expected_work_path}/file.txt", contents="old")
        fs.create_file(f"{expected_work_path}/old.txt", contents="should be removed")

        patch_fake_download(mocker, fs, FakeDownloadMode.OVERWRITE)

        # Act
        runner.prepare_test_case(config, testcase_logger)
        runner.run_test_case(config=config, run_data=run_data)

        # Assert
        with open(f"{expected_work_path}/file.txt") as f:
            assert f.read() == "new"
        assert not fs.exists(f"{expected_work_path}/old.txt")

    def test_run_prepares_dvc_cases_before_dispatch(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.parallel = True

        config1 = TestComparisonRunner.create_test_case_config("Name_1", testcase_path=TestCasePath("path1", "DVC"))
        config2 = TestComparisonRunner.create_test_case_config("Name_2", testcase_path=TestCasePath("path2", "DVC"))
        settings.configs_to_run = [config1, config2]

        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        call_order: List[str] = []

        mocker.patch.object(runner, "_TestSetRunner__update_programs", return_value=[])
        mocker.patch.object(runner, "_TestSetRunner__download_dependencies")
        mocker.patch.object(runner, "show_summary", return_value=None)
        prepare_mock = mocker.patch.object(
            runner,
            "prepare_test_case",
            side_effect=lambda config, _logger: call_order.append(f"prepare:{config.name}"),
        )
        parallel_mock = mocker.patch.object(
            runner,
            "run_tests_in_parallel",
            side_effect=lambda: call_order.append("dispatch:parallel") or [MagicMock()],
        )

        # Act
        runner.run()

        # Assert
        assert call_order == ["prepare:Name_1", "prepare:Name_2", "dispatch:parallel"]
        prepare_mock.assert_has_calls([call(config1, logger), call(config2, logger)])
        parallel_mock.assert_called_once()

    def test_run_test_case_raises_error_when_paths_not_prepared(self) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()

        logger = MagicMock(spec=ConsoleLogger)
        test_case_logger = MagicMock()
        logger.create_test_case_logger.return_value = test_case_logger

        runner = ComparisonRunner(settings, logger)

        # Create a DVC config so preparation is skipped in run_test_case
        config = TestComparisonRunner.create_test_case_config("Name_1", testcase_path=TestCasePath("path", "DVC"))
        config.absolute_test_case_path = ""
        config.absolute_test_case_reference_path = ""

        run_data = RunData(1, 1)

        # Act
        runner.run_test_case(config, run_data)

        # Assert
        assert test_case_logger.exception.called
        exception_args = test_case_logger.exception.call_args[0]
        assert "Test case paths are not prepared" in str(exception_args[0])

    def test_run_logs_error_when_prepare_test_case_fails(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.parallel = False

        config = TestComparisonRunner.create_test_case_config("Name_1", testcase_path=TestCasePath("path", "DVC"))
        settings.configs_to_run = [config]

        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        mocker.patch.object(runner, "_TestSetRunner__update_programs", return_value=[])
        mocker.patch.object(runner, "_TestSetRunner__download_dependencies")
        mocker.patch.object(runner, "run_tests_sequentially", return_value=[MagicMock()])
        mocker.patch.object(runner, "show_summary", return_value=None)

        test_exception = Exception("Test preparation failed")

        mocker.patch.object(runner, "prepare_test_case", side_effect=test_exception)
        cleanup_mock = mocker.patch.object(runner, "cleanup_failed_preparation")

        # Act
        runner.run()

        # Assert
        expected_error_message = f"Failed to prepare test case 'Name_1': {test_exception}"
        logger.error.assert_called_with(expected_error_message)
        cleanup_mock.assert_called_once_with(config)

    def test_cleanup_failed_preparation_removes_directories(self, fs: FakeFilesystem) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        logger = MagicMock(spec=ConsoleLogger)
        runner = ComparisonRunner(settings, logger)

        config = TestComparisonRunner.create_test_case_config("Name_1")

        work_dir = "/cases/win64/Name_1_work"
        input_dir = "/cases/win64/Name_1"
        ref_dir = "/refs/win64/Name_1"

        fs.create_dir(work_dir)
        fs.create_dir(input_dir)
        fs.create_dir(ref_dir)

        config.absolute_test_case_path = work_dir
        config.absolute_test_case_reference_path = ref_dir

        # Act
        runner.cleanup_failed_preparation(config)

        # Assert
        assert fs.exists(work_dir), "Work directory should NOT be removed"
        assert not fs.exists(input_dir), "Input directory should be removed"
        assert not fs.exists(ref_dir), "Reference directory should be removed"

    @staticmethod
    def create_test_case_config(
        name: str,
        ignore_testcase: bool = False,
        locations: List[Location] | None = None,
        testcase_path: TestCasePath | None = None,
    ) -> TestCaseConfig:
        config = TestCaseConfig()
        config.name = name
        config.ignore = ignore_testcase

        if testcase_path is None:
            config.path = TestCasePath("", "")
        else:
            config.path = testcase_path

        if locations is None:
            locations = []
        else:
            config.locations = locations

        return config

    @staticmethod
    def create_location(
        name: str,
        location_type: PathType = PathType.INPUT,
        root: str = "https://deltares.nl/",
        from_path: str | None = None,
    ) -> Location:
        location = Location()
        location.name = name
        location.root = root
        location.from_path = from_path if from_path is not None else "win64"
        location.type = location_type
        return location

    @staticmethod
    def clean_empty_logs(filenames: str) -> None:
        try:
            for filename in glob.glob(filenames.split(".")[0]):
                os.remove(filename)
        except OSError:
            pass

    @staticmethod
    def assert_is_file(path: str) -> None:
        assert pl.Path(path).resolve().is_file(), f"File does not exist: {str(path)}"
