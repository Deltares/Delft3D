import sys
import pytest
from unittest.mock import ANY, Mock, patch
from src.utils.test_bench_parameter_parser import TestBenchParameterParser


class TestTestBenchParameterParser():
    @pytest.fixture()
    def override_command_line_args(self):
        temp = sys.argv
        sys.argv = ["arg1",
                    "--compare",]
        yield sys.argv
        sys.argv = temp

    @pytest.fixture()
    def override_command_line_args_with_server_base_url(self, override_command_line_args):
        override_command_line_args.extend([
                    "--server-base-url",
                    "https://abcdef.ij"])
        yield sys.argv

    @staticmethod
    def test_parse_arguments_default_server_base_url(override_command_line_args):
        with patch('src.utils.test_bench_parameter_parser.XmlConfigParser') as XmlConfigParserMock:
            # Arrange
            xmlConfigParser_instance = Mock()
            XmlConfigParserMock.return_value = xmlConfigParser_instance
            xmlConfigParser_instance.load.return_value = [None, [], []]

            # Act
            TestBenchParameterParser.parse_arguments_to_settings()

            # Assert
            xmlConfigParser_instance.load.assert_called_once_with(ANY, ANY, ANY, "https://repos.deltares.nl/repos/DSCTestbench/trunk")

    @staticmethod
    def test_parse_arguments_override_server_base_url(override_command_line_args_with_server_base_url):
        with patch('src.utils.test_bench_parameter_parser.XmlConfigParser') as XmlConfigParserMock:
            # Arrange
            xmlConfigParser_instance = Mock()
            XmlConfigParserMock.return_value = xmlConfigParser_instance
            xmlConfigParser_instance.load.return_value = [None, [], []]

            # Act
            TestBenchParameterParser.parse_arguments_to_settings()

            # Assert
            xmlConfigParser_instance.load.assert_called_once_with(ANY, ANY, ANY, "https://abcdef.ij")
