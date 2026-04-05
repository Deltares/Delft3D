import os
from typing import Optional

from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture

from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.path_type import PathType
from src.suite.reference_runner import ReferenceRunner
from src.suite.run_data import RunData
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.i_main_logger import IMainLogger


class TestReferenceRunner:
    def test_post_process__copy_case_content(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        test_name = "name_1"
        platform = "win64" if os.name == "nt" else "lnx64"
        config = TestReferenceRunner.create_test_case_config(test_name, False, platform=platform)
        run_data = mocker.Mock(spec=RunData)
        case_logger = mocker.Mock(spec=FileLogger)
        main_logger = mocker.Mock(spec=IMainLogger)
        settings = mocker.Mock(spec=TestBenchSettings)
        mocker.patch("src.suite.test_set_runner.HandlerFactory")
        TestReferenceRunner.simulate_run(test_name, fs)

        # Act
        reference_runner = ReferenceRunner(settings, main_logger)
        reference_runner.post_process(config, case_logger, run_data)

        # Assert
        assert fs.exists(f"/reference/{platform}/name_1/output"), "Folder was not succesfully created."
        assert fs.exists(f"/reference/{platform}/name_1/output/out1"), "Folder content was not succesfully created"

    def test_post_process__overwrite_reference_content(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        test_name = "name_1"
        platform = "win64" if os.name == "nt" else "lnx64"
        config = TestReferenceRunner.create_test_case_config(test_name, False, platform=platform)
        run_data = mocker.Mock(spec=RunData)
        case_logger = mocker.Mock(spec=FileLogger)
        main_logger = mocker.Mock(spec=IMainLogger)
        settings = mocker.Mock(spec=TestBenchSettings)
        mocker.patch("src.suite.test_set_runner.HandlerFactory")
        TestReferenceRunner.simulate_run(test_name, fs)
        TestReferenceRunner.simulate_run(test_name, fs, location=f"reference/{platform}")

        # Act
        reference_runner = ReferenceRunner(settings, main_logger)
        reference_runner.post_process(config, case_logger, run_data)

        # Assert
        with open(f"reference/{platform}/{test_name}/output/out1") as file:
            content = file.readlines()
            assert content[0] == "case simulated", "Reference was not overwritten with new case output data."

    def test_show_summary__after_post_process(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        test_name = "name_1"
        platform = "win64" if os.name == "nt" else "lnx64"
        config = TestReferenceRunner.create_test_case_config(test_name, False, platform=platform)
        run_data = mocker.Mock(spec=RunData)
        case_logger = mocker.Mock(spec=FileLogger)
        main_logger = mocker.Mock(spec=IMainLogger)
        settings = mocker.Mock(spec=TestBenchSettings)
        mocker.patch("src.suite.test_set_runner.HandlerFactory")
        TestReferenceRunner.simulate_run(test_name, fs)
        TestReferenceRunner.simulate_run(test_name, fs, location=f"reference/{platform}")

        # Act
        reference_runner = ReferenceRunner(settings, main_logger)
        result = reference_runner.post_process(config, case_logger, run_data)
        reference_runner.show_summary([result], main_logger)

        # Assert
        info = main_logger.info.call_args_list
        log_result_msg = TestReferenceRunner.result_log(test_name, config.run_time, config.ref_run_time, "OK")
        assert mocker.call(log_result_msg) in info, f"couldn't find OK result for copying of reference for {test_name}"

    @staticmethod
    def create_test_case_config(
        name: str, ignore: bool, path_type: Optional[PathType] = PathType.REFERENCE, platform: Optional[str] = "lnx64"
    ) -> TestCaseConfig:
        config = TestCaseConfig()
        config.name = name
        config.ignore = ignore
        config.absolute_test_case_path = f"/case/{name}"
        config.absolute_test_case_reference_path = f"/reference/{platform}/{name}"
        config.run_time = 1.238615243
        config.ref_run_time = 100.0

        location = TestReferenceRunner.create_location(name, path_type)
        config.locations = [location]
        config.path = TestCasePath("test")

        return config

    @staticmethod
    def create_location(name: str, path_type: PathType) -> Location:
        location = Location()
        location.root = "https://deltares.nl/"
        location.from_path = name.replace(" ", "")
        location.type = path_type
        return location

    @staticmethod
    def simulate_run(name: str, fs: FakeFilesystem, location: Optional[str] = "case") -> None:
        case_path = f"/{location}/{name}"
        text = TestReferenceRunner.tb3_char_text()
        fs.create_dir(case_path)
        # INPUT
        fs.create_file(f"{case_path}/file1", contents="foo")

        # OUTPUT
        fs.create_dir(f"{case_path}/output")
        fs.create_file(f"{case_path}/result.txt", contents="test_run = OK")
        fs.create_file(f"{case_path}/output/out1", contents=f"{location} simulated")
        fs.create_file(f"{case_path}/_tb3_char.run", contents=text)

    @staticmethod
    def tb3_char_text() -> str:
        return "\n".join(
            [
                "Start_size:146335",
                "Runtime:6.851168632507324",
                "Output_added:output",
                "Output_added:result.txt",
                "Output_added:_tb3_char.run",
                "End_size:215861",
            ]
        )

    @staticmethod
    def result_log(name, run_time, ref_run_time, result) -> str:
        return "%-40s (%7.2f %7.2f) %-6s" % (
            name[:40],
            run_time,
            run_time / ref_run_time,
            result,
        )
