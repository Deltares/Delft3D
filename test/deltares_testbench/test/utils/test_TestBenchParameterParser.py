import sys
from pathlib import Path

import pytest

from src.utils.test_bench_parameter_parser import TestBenchParameterParser


class TestTestBenchParameterParser:
    @pytest.fixture()
    def override_command_line_args(self):
        temp = sys.argv
        sys.argv = [
            "arg1",
            "--compare",
        ]
        yield sys.argv
        sys.argv = temp

    @pytest.fixture()
    def override_command_line_args_with_server_base_url(self, override_command_line_args):
        override_command_line_args.extend(["--server-base-url", "https://abcdef.ij"])
        return sys.argv

    @pytest.fixture()
    def override_command_line_args_with_filter(self, override_command_line_args):
        def _set_filter(value: str):
            override_command_line_args.extend(["--filter", value])
            return sys.argv

        return _set_filter

    @staticmethod
    def test_parse_arguments_default_server_base_url(override_command_line_args) -> None:
        # Arrange
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert
        assert settings.server_base_url == "https://s3.deltares.nl/dsc-testbench"

    @staticmethod
    def test_parse_arguments_override_server_base_url(
        override_command_line_args_with_server_base_url,
    ) -> None:
        # Arrange
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert
        assert settings.server_base_url == "https://abcdef.ij"

    @staticmethod
    def test_filter_csv_extracts_failed_tests(
        override_command_line_args_with_filter, tmp_path: Path
    ) -> None:
        # Arrange
        csv_file = tmp_path / "tests.csv"
        csv_file.write_text(
            "Order#,Test Name,Status,Duration(ms)\n"
            "1,test_passing,OK,100\n"
            "2,test_failing_a,Failure,200\n"
            "3,test_failing_b,Failure,300\n",
            encoding="utf-8",
        )
        override_command_line_args_with_filter(str(csv_file))
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert
        assert settings.filter == "testcase=test_failing_a,test_failing_b"

    @staticmethod
    def test_filter_csv_with_no_failures_returns_path_unchanged(
        override_command_line_args_with_filter, tmp_path: Path
    ) -> None:
        # Arrange
        csv_file = tmp_path / "tests.csv"
        csv_file.write_text(
            "Order#,Test Name,Status,Duration(ms)\n"
            "1,test_passing_a,OK,100\n"
            "2,test_passing_b,OK,200\n",
            encoding="utf-8",
        )
        override_command_line_args_with_filter(str(csv_file))
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert — no failures found, falls back to the raw argument
        assert settings.filter == str(csv_file)

    @staticmethod
    def test_filter_plain_string_is_passed_through(
        override_command_line_args_with_filter,
    ) -> None:
        # Arrange
        override_command_line_args_with_filter("testcase=some_test")
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert
        assert settings.filter == "testcase=some_test"

    @staticmethod
    def test_filter_empty_string_is_passed_through(override_command_line_args) -> None:
        # Arrange — no --filter argument supplied, default is ""
        parser = TestBenchParameterParser()

        # Act
        settings = parser.parse_arguments_to_settings()

        # Assert
        assert settings.filter == ""

    @staticmethod
    def test_filter_csv_missing_required_columns_raises(
        override_command_line_args_with_filter, tmp_path: Path
    ) -> None:
        # Arrange
        csv_file = tmp_path / "tests.csv"
        csv_file.write_text(
            "Order#,TestName,Result\n"
            "1,test_a,Failure\n",
            encoding="utf-8",
        )
        override_command_line_args_with_filter(str(csv_file))
        parser = TestBenchParameterParser()

        # Act / Assert
        with pytest.raises(ValueError, match="missing required columns"):
            parser.parse_arguments_to_settings()