import sys

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
