import pytest
import textwrap
import numpy as np
from unittest.mock import Mock, patch, call
from src.utils.logging.log_level import LogLevel
from src.utils.common import log_header, log_separator, log_table, escape_teamcity


class TestCommon:
    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_header(self, mock_logger):
        logger_instance = Mock()

        # Arrange
        mock_logger.return_value = logger_instance

        # Act
        log_header("Sample Header", logger_instance, LogLevel.INFO, 10, "*")

        # Assert
        expected_calls = [
            call('**********', LogLevel.INFO),
            call('Sample Header', LogLevel.INFO),
            call('**********', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_separator(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        # Act
        log_separator(logger_instance, LogLevel.INFO, 3, 'v')

        # Assert
        logger_instance.log.assert_called_with('vvv', LogLevel.INFO)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_log_table(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        sample_table = {
            "Header 1": ["2", "5"],
            "Header 2": ["test string", "test 2"],
            "Header 3": ["3.9", "2.6"],
        }

        # Act
        log_table(sample_table, logger_instance, LogLevel.INFO, "-")

        # Assert
        expected_calls = [
            call('-------------------------------', LogLevel.INFO),
            call('|Header 1|Header 2   |Header 3|', LogLevel.INFO),
            call('-------------------------------', LogLevel.INFO),
            call('|2       |test string|3.9     |', LogLevel.INFO),
            call('|5       |test 2     |2.6     |', LogLevel.INFO),
            call('-------------------------------', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)

    @patch('src.utils.logging.console_logger.ConsoleLogger')
    def test_convert_numpy_float_to_python_float(self, mock_logger):
        # Arrange
        logger_instance = Mock()
        mock_logger.return_value = logger_instance

        sample_table = {
            "Test name": [],
            "Runtime": [],
            "MaxAbsDiff": [],
            "File name": [],
        }

        sample_table["Test name"].append("name_1")
        sample_table["Runtime"].append("1.177e+00")
        sample_table["MaxAbsDiff"].append(np.float64(8.881784197001252e-16))
        sample_table["File name"].append("abc.nc")

        # Act
        log_table(sample_table, logger_instance, LogLevel.INFO, "-")

        # Assert
        expected_calls = [
            call('------------------------------------------', LogLevel.INFO),
            call('|Test name|Runtime  |MaxAbsDiff|File name|', LogLevel.INFO),
            call('------------------------------------------', LogLevel.INFO),
            call('|name_1   |1.177e+00|8.882e-16 |abc.nc   |', LogLevel.INFO),
            call('------------------------------------------', LogLevel.INFO),
        ]

        logger_instance.log.assert_has_calls(expected_calls)

    @pytest.mark.parametrize(
        "input,expected",
        [
            pytest.param("\n", "|n", id="line-feed"),
            pytest.param("\r", "|r", id="cariage-return"),
            pytest.param("[", "|[", id="open-bracket"),
            pytest.param("]", "|]", id="close-bracket"),
            pytest.param("'", "|'", id="single-quote"),
            pytest.param("|", "||", id="vertical-bar"),
            pytest.param("\u265e", "|0x265E", id="unicode"),
            pytest.param("\u00e9", "|0x00E9", id="zero-padded-unicode"),
        ],
    )
    def test_escape_teamcity__single_characters(self, input: str, expected: str) -> None:
        assert escape_teamcity(input) == expected

    def test_escape_teamcity__replace_in_long_string(self) -> None:
        # Arrange
        text = textwrap.dedent(
            f"""
            In this text, there are newlines (
            ), cariage-returns (\r), single quotes ('), brackets ([]), vertical bars (|).
            Finally there are some unicode characters: ({"".join(chr(c) for c in range(0x2654, 0x2660))}).
            Watch out for those unicode characters with codes in the lower range. The code must be
            exactly four characters long. So remember to pad the code with zeroes. Sincerely, Ren\u00e9.
            """,
        ).strip()  # Strip off leading and trailing line feeds from multi-line string.
        expected = textwrap.dedent(
           f"""
            In this text, there are newlines (
            ), cariage-returns (|r), single quotes (|'), brackets (|[|]), vertical bars (||).
            Finally there are some unicode characters: ({"".join("|0x{:04X}".format(c) for c in range(0x2654, 0x2660))}).
            Watch out for those unicode characters with codes in the lower range. The code must be
            exactly four characters long. So remember to pad the code with zeroes. Sincerely, Ren|0x00E9.
            """,
        ).strip().replace("\n", "|n")

        # Act
        result = escape_teamcity(text)

        # Assert
        assert result == expected