import logging
from logging.handlers import RotatingFileHandler
import platform
import stat
import tempfile
from pathlib import Path
from typing import Iterator

import pytest
from pytest_mock import MockerFixture

from src.config.program_config import ProgramConfig
from src.suite.program import Program
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.i_logger import ILogger


@pytest.fixture
def tmp_dir() -> Iterator[Path]:
    """Create temporary directory that is cleaned up after tests."""
    with tempfile.TemporaryDirectory() as tmp_dir_name:
        tmp_dir = Path(tmp_dir_name)
        yield tmp_dir


def close_file_logger_handler(logger: logging.Logger) -> None:
    """Program logs output to a logger with a file handler. Close the file."""
    file_handler, *other_handlers = logger.handlers
    assert not other_handlers
    assert isinstance(file_handler, RotatingFileHandler)
    file_handler.close()


class TestProgram:

    def test_run__log_output_to_file(self, mocker: MockerFixture, tmp_dir: Path) -> None:
        # Arrange
        system = platform.system()
        if system == "Linux":
            script = tmp_dir / "foo.sh"
            script.write_text("#!/bin/bash\necho 'foo!'")  # Script that echos something.
            script.chmod(stat.S_IFREG | 0o755)  # Make script executable.
        elif system == "Windows":
            script = tmp_dir / "foo.bat"
            script.write_text("@echo off\necho \"foo!\"")  # Script that echos something.
        else:
            pytest.skip(reason=f"Unknown platform: {system}")

        config = ProgramConfig()
        config.name = "foo"
        config.working_directory = str(tmp_dir.absolute())
        config.absolute_bin_path = str(script.absolute())
        config.log_output_to_file = True
        program = Program(config, TestBenchSettings())
        logger = mocker.Mock(spec=ILogger)

        # Act
        program.run(logger)

        # Assert
        log_file, *other_files = tmp_dir.glob("foo=*.log")
        assert not other_files
        assert "foo!" in log_file.read_text()

        # Close the file in the file logger handler to avoid errors.
        close_file_logger_handler(logging.getLogger(log_file.name))    

    @pytest.mark.skipif(platform.system() != "Linux", reason="Test is specific for Linux")
    def test_run__linux_no_execute_permissions__log_exception(self, mocker: MockerFixture, tmp_dir: Path) -> None:
        # Arrange
        script = tmp_dir / "foo.sh"
        script.write_text("#!/bin/bash\necho 'foo!'")  # Script that echos something.

        config = ProgramConfig()
        config.name = "foo"
        config.working_directory = str(tmp_dir.absolute())
        config.absolute_bin_path = str(script.absolute())
        config.log_output_to_file = True
        program = Program(config, TestBenchSettings())
        logger = mocker.Mock(spec=ILogger)

        # Act
        program.run(logger)

        # Assert
        logger.exception.assert_called_once()
        assert "permission" in logger.exception.call_args.args[0].lower()
