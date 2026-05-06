import io
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
    def test_read_failed_tests_from_csv_returns_failed_names() -> None:
        csv_content = io.StringIO(
            "Order#,Test Name,Status,Duration(ms)\n"
            "1,test_passing,OK,100\n"
            "2,test_failing_a,Failure,200\n"
            "3,test_failing_b,Failure,300\n"
        )

        result = TestBenchParameterParser.read_failed_tests_from_csv(csv_content)

        assert result == ["test_failing_a", "test_failing_b"]

    @staticmethod
    def test_read_failed_tests_from_csv_no_failures_returns_empty() -> None:
        csv_content = io.StringIO(
            "Order#,Test Name,Status,Duration(ms)\n" "1,test_passing_a,OK,100\n" "2,test_passing_b,OK,200\n"
        )

        result = TestBenchParameterParser.read_failed_tests_from_csv(csv_content)

        assert result == []

    @staticmethod
    def test_read_failed_tests_from_csv_missing_columns_raises() -> None:
        csv_content = io.StringIO("Order#,TestName,Result\n" "1,test_a,Failure\n")

        with pytest.raises(ValueError, match="missing required columns"):
            TestBenchParameterParser.read_failed_tests_from_csv(csv_content)

    @staticmethod
    def test_filter_csv_nonexistent_file_aborts(override_command_line_args) -> None:
        override_command_line_args.extend(["--filter-tc-csv", "/nonexistent/path/tests.csv"])

        with pytest.raises(SystemExit) as exc_info:
            TestBenchParameterParser().parse_arguments_to_settings()

        assert exc_info.value.code == 2

    @staticmethod
    def test_filter_and_filter_csv_are_mutually_exclusive(override_command_line_args, tmp_path: Path) -> None:
        csv_file = tmp_path / "tests.csv"
        csv_file.write_text("Order#,Test Name,Status\n1,test_a,Failure\n", encoding="utf-8")
        override_command_line_args.extend(["--filter", "testcase=something", "--filter-tc-csv", str(csv_file)])

        with pytest.raises(SystemExit) as exc_info:
            TestBenchParameterParser().parse_arguments_to_settings()

        assert exc_info.value.code == 2
