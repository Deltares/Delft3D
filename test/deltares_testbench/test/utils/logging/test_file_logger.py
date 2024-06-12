import os
import uuid

from pyfakefs.fake_filesystem import FakeFilesystem

from src.utils.logging.file_logger import FileLogger
from src.utils.logging.log_level import LogLevel


class TestFileLogger:
    def test_init_logger_should_create_1_log_file(self, fs: FakeFilesystem):
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

        with open(log_path, 'r') as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."

    def test_init_logger_should_back_up_old_logs_with_same_name(self, fs: FakeFilesystem):
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

        with open(log_path_back_up, 'r') as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message1 in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."

        with open(log_path, 'r') as log_file:
            log_contents = log_file.readlines()
            relevant_lines = [line for line in log_contents if test_message2 in line]
            assert len(relevant_lines) > 0, "The log file does not contain the expected message."
