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


def patch_fake_download(mocker: MockerFixture, fs: FakeFilesystem, mode: FakeDownloadMode) -> None:
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

    mocker.patch("src.suite.test_set_runner.HandlerFactory.download", side_effect=_fake_download)


class TestComparisonRunner:
    @pytest.mark.usefixtures("fs")  # Use fake filesystem.
    def test_run_tests_and_debug_log_downloaded_file(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_download = []
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config("Name_1", locations=[ref_location, case_location])
        config.path = TestCasePath("abc/prefix", "v1")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        download_mock = mocker.patch("src.suite.test_set_runner.HandlerFactory.download")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        ref_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("references", "win64", "Name_1"))
        case_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("cases", "win64", "Name_1"))
        expected_log_message1 = f"Downloading reference result, {ref_path} from https://deltares.nl/win64/abc/prefix"
        expected_log_message2 = f"Downloading input of case, {case_path} from https://deltares.nl/win64/abc/prefix"
        assert call(expected_log_message1) in testcase_logger.debug.call_args_list
        assert call(expected_log_message2) in testcase_logger.debug.call_args_list
        assert download_mock.call_count == 2  # Downloads case AND reference data.

    def test_log_and_skip_with_argument_skip_run(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_download = list(PathType)  # Please skip downloading anything.
        ref_location = TestComparisonRunner.create_location(name="reference", location_type=PathType.REFERENCE)
        case_location = TestComparisonRunner.create_location(name="case", location_type=PathType.INPUT)
        config = TestComparisonRunner.create_test_case_config("Name_1", locations=[ref_location, case_location])
        config.path = TestCasePath("abc/prefix", "vl")
        settings.configs_to_run = [config]
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        prepare_mock = mocker.patch("src.suite.test_set_runner.TestSetRunner._TestSetRunner__prepare_test_case")
        run_mock = mocker.patch("src.suite.test_case.TestCase.run")
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        expected_log_message = "Skipping execution of testcase (postprocess only)...\n"
        assert call(expected_log_message) in testcase_logger.info.call_args_list
        prepare_mock.assert_called()
        run_mock.assert_not_called()

    def test_prepare_case_uses_minio(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths()
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_download = []
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
        download_mock = mocker.patch("src.suite.test_set_runner.HandlerFactory.download")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        ref_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("references", "win64", "testname"))
        case_path = Paths().rebuildToLocalPath(Paths().mergeFullPath("cases", "win64", "testname"))
        expected_log_message1 = f"Downloading reference result, {ref_path} from https://deltares.nl/win64/abc/prefix"
        expected_log_message2 = f"Downloading input of case, {case_path} from https://deltares.nl/win64/abc/prefix"
        assert call(expected_log_message1) in testcase_logger.debug.call_args_list
        assert call(expected_log_message2) in testcase_logger.debug.call_args_list
        assert download_mock.call_count == 2  # Downloads case AND reference data.

    def test_prepare_case_uses_dvc(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.local_paths = LocalPaths(cases_path="data/cases", references_path="data/cases")
        settings.command_line_settings.skip_run = True
        settings.command_line_settings.skip_download = []
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
        download_mock = mocker.patch("src.suite.test_set_runner.HandlerFactory.download")

        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_sequentially()

        # Assert
        remote_ref_path = os.path.abspath("data/cases/abc/prefix/reference_win64.dvc")
        remote_case_path = os.path.abspath("data/cases/abc/prefix/input.dvc")
        expected_log_message1 = f"Downloading reference result, from DVC file at {remote_ref_path}"
        expected_log_message2 = f"Downloading input of case, from DVC file at {remote_case_path}"
        assert call(expected_log_message1) in testcase_logger.debug.call_args_list
        assert call(expected_log_message2) in testcase_logger.debug.call_args_list
        assert download_mock.call_count == 2

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
        settings.command_line_settings.skip_download = list(PathType)  # Please skip downloading anything.
        logger = ConsoleLogger(LogLevel.INFO)
        runner = ComparisonRunner(settings, logger)

        # Act
        runner.run_tests_in_parallel()

        # Assert
        TestComparisonRunner.assertIsFile(log_file_1)
        TestComparisonRunner.assertIsFile(log_file_2)

    def test_run_without_test_cases_logs_no_results(self, mocker: MockerFixture) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = list(PathType)  # Please skip downloading anything.
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
                f"There are no test cases in '{settings.command_line_settings.config_file}' "
                f"with applied filter '{settings.command_line_settings.filter}'."
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
        settings.command_line_settings.skip_download = list(PathType)  # Please skip downloading anything.
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
                f"There are no test cases in '{settings.command_line_settings.config_file}' with applied filter '{settings.command_line_settings.filter}'."
            )
            in logger.error.call_args_list
        )

    def test_run_tests_sequentially__run_multiple__continue_on_error(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        # Create settings
        settings = TestBenchSettings()
        settings.command_line_settings.skip_run = False
        settings.command_line_settings.parallel = False
        settings.local_paths = LocalPaths()
        # Ghastly trick to ensure that the unit test doesn't make any actual web requests.
        settings.command_line_settings.skip_download = list(PathType)

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

        # Accursed, unutterable `patch`-ing and creating fake directories just so the unit test doesn't crash.
        fs.makedirs("/cases/win64/i_am_error")
        fs.makedirs("/cases/win64/i_always_succeed")
        mocker.patch("src.suite.test_case.Program.run")  # Patch `Program.run` so it does nothing
        return_code_mock = mocker.patch(
            "src.suite.test_case.Program.last_return_code", new_callable=PropertyMock, side_effect=[1, 0]
        )
        # Make `getError` first return an error, then no error.
        return_values = iter([RuntimeError("Failed to frobnicate"), None])
        mocker.patch("src.suite.test_case.Program.getError", side_effect=lambda: next(return_values))
        # Make the return code of the program `1`, and then `0`.
        return_code_mock.side_effect = [1, 0]

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
        run_data = RunData(1, 10)

        patch_fake_download(mocker, fs, FakeDownloadMode.ALL)

        expected_work_path = "/cases/win64/Banana_1_work"

        # Act
        runner.run_test_case(config=config, run_data=run_data)

        # Assert
        assert fs.exists(expected_work_path)

    def test_copy_to_work_folder__missing_source__warns_and_returns(
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

        patch_fake_download(mocker, fs, FakeDownloadMode.REFS_ONLY)

        # Make the input path exist but not be a directory.
        fs.create_file(expected_local_input_path, contents="not a directory")

        # Act
        result = runner.run_test_case(config=config, run_data=run_data)

        # Assert
        assert any(
            "NotADirectoryError" in str(call.args[0])
            and "Expected a directory to copy to work folder" in str(call.args[0])
            for call in testcase_logger.exception.call_args_list
        ), "Expected NotADirectoryError to be logged"
        assert result.results[0][-1].result == EndResult.ERROR
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
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger
        runner = ComparisonRunner(settings, logger)
        run_data = RunData(1, 1)

        expected_local_input_path = "/cases/win64/Name_1"
        expected_work_path = expected_local_input_path + "_work"

        patch_fake_download(mocker, fs, FakeDownloadMode.FILES)

        # Act
        runner.run_test_case(config=config, run_data=run_data)

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
        runner.run_test_case(config=config, run_data=run_data)

        # Assert
        with open(f"{expected_work_path}/file.txt") as f:
            assert f.read() == "new"
        assert not fs.exists(f"{expected_work_path}/old.txt")

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
        location.root = root
        location.from_path = "win64"
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
    def assertIsFile(path: str) -> None:
        assert pl.Path(path).resolve().is_file(), f"File does not exist: {str(path)}"
