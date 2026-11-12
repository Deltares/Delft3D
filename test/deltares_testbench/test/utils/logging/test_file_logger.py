import os
import uuid
from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from testbench.utils.common import get_default_logging_folder_path
from testbench.utils.logging.file_logger import FileLogger
from testbench.utils.logging.log_level import LogLevel


class TestFileLogger:
    def test_default_logs_are_written_in_working_directory(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        monkeypatch.chdir(tmp_path)
        log_path = os.path.join(get_default_logging_folder_path(), "file.log")

        logger = FileLogger(LogLevel.INFO, "working_directory_logger", log_path)
        try:
            logger.info("installed package log")
        finally:
            logger.close()

        assert "installed package log" in (tmp_path / "logs" / "file.log").read_text(encoding="utf-8")

    def test_init_logger_should_create_1_log_file(self, fs: FakeFilesystem) -> None:
        # Arrange
        random_hash = str(uuid.uuid4())
        log_folder = os.path.join(os.curdir, random_hash)
        log_path = os.path.join(log_folder, "file.log")
        fs.create_dir(log_folder)
        test_message = "hello-world"

        # Act
        logger = FileLogger(LogLevel.INFO, "FileLogger", log_path)
        logger.info(test_message)
        logger.close()

        # Assert
        assert len(os.listdir(log_folder)) == 1, "There should be only one log file."
        assert os.path.isfile(log_path), "The log file does not exist."

        with open(log_path, "r") as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."

    def test_init_logger_should_back_up_old_logs_with_same_name(self, fs: FakeFilesystem) -> None:
        # Arrange
        random_hash = str(uuid.uuid4())
        log_folder = os.path.join(os.curdir, random_hash)
        log_path = os.path.join(log_folder, "file.log")
        log_path_back_up = os.path.join(log_folder, "file.log.1")
        fs.create_dir(log_folder)
        test_message1 = "hello-world"
        test_message2 = "hello-space"

        # Act
        logger1 = FileLogger(LogLevel.INFO, "FileLogger", log_path)
        logger1.info(test_message1)
        logger1.close()

        logger2 = FileLogger(LogLevel.INFO, "FileLogger", log_path)
        logger2.info(test_message2)
        logger2.close()

        # Assert
        assert len(os.listdir(log_folder)) == 2, "There should be only be 2 log file."
        assert os.path.isfile(log_path), "The log file does not exist."
        assert os.path.isfile(log_path_back_up), "The log file does not exist."

        with open(log_path_back_up, "r") as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message1 in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."

        with open(log_path, "r") as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message2 in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."
