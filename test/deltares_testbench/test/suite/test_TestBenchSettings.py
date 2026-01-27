from unittest.mock import MagicMock, Mock

from pytest_mock import MockerFixture

from src.config.local_paths import LocalPaths
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.types.path_type import PathType
from src.suite.command_line_settings import CommandLineSettings
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.i_logger import ILogger
from src.utils.xml_config_parser import XmlConfig


class TestTestBenchSettings:
    def test_default_settings(self) -> None:
        # Arrange
        settings = TestBenchSettings()

        # Assert
        assert isinstance(settings.command_line_settings, CommandLineSettings)
        assert settings.configs_to_run == []
        assert settings.programs == []
        # local_paths is not initialized by default, only set during setup_runtime_settings

    def test_setup_runtime_settings(self, mocker: MockerFixture) -> None:
        # Arrange
        test_bench_settings = TestBenchSettings()
        command_line_settings = CommandLineSettings()
        command_line_settings.filter = "test_filter"

        mock_local_paths = Mock(spec=LocalPaths)
        mock_program_configs = [Mock(spec=ProgramConfig), Mock(spec=ProgramConfig)]
        mock_testcase_configs = [Mock(spec=TestCaseConfig), Mock(spec=TestCaseConfig)]
        mock_filtered_configs = [Mock(spec=TestCaseConfig)]

        xml_config = Mock(spec=XmlConfig)
        xml_config.local_paths = mock_local_paths
        xml_config.program_configs = mock_program_configs
        xml_config.testcase_configs = mock_testcase_configs

        mock_logger = Mock(spec=ILogger)

        mock_filter_configs = mocker.patch("src.utils.xml_config_parser.XmlConfigParser.filter_configs")
        mock_filter_configs.return_value = mock_filtered_configs

        # Act
        test_bench_settings.setup_runtime_settings(command_line_settings, xml_config, mock_logger)

        # Assert
        assert test_bench_settings.command_line_settings == command_line_settings
        assert test_bench_settings.local_paths == mock_local_paths
        assert test_bench_settings.programs == mock_program_configs
        assert test_bench_settings.configs_to_run == mock_filtered_configs

        mock_filter_configs.assert_called_once_with(mock_testcase_configs, "test_filter", mock_logger)

    def test_setup_runtime_settings_with_config_filtering(self, mocker: MockerFixture) -> None:
        # Arrange
        test_bench_settings = TestBenchSettings()
        command_line_settings = CommandLineSettings()
        command_line_settings.filter = "testcase=example"

        config1 = Mock(spec=TestCaseConfig)
        config1.name = "example_test_case_1"
        config2 = Mock(spec=TestCaseConfig)
        config2.name = "other_test_case_2"
        config3 = Mock(spec=TestCaseConfig)
        config3.name = "example_test_case_3"

        mock_local_paths = Mock(spec=LocalPaths)
        mock_program_configs = [Mock(spec=ProgramConfig)]
        all_testcase_configs = [config1, config2, config3]

        xml_config = Mock(spec=XmlConfig)
        xml_config.local_paths = mock_local_paths
        xml_config.program_configs = mock_program_configs
        xml_config.testcase_configs = all_testcase_configs

        mock_logger = Mock(spec=ILogger)

        expected_filtered_configs = [config1, config3]  # Only configs with "example" in name
        mock_filter_configs = mocker.patch("src.utils.xml_config_parser.XmlConfigParser.filter_configs")
        mock_filter_configs.return_value = expected_filtered_configs

        # Act
        test_bench_settings.setup_runtime_settings(command_line_settings, xml_config, mock_logger)

        # Assert
        assert test_bench_settings.configs_to_run == expected_filtered_configs
        assert len(test_bench_settings.configs_to_run) == 2

        mock_filter_configs.assert_called_once_with(all_testcase_configs, "testcase=example", mock_logger)

        config_names = [config.name for config in test_bench_settings.configs_to_run]
        assert "example_test_case_1" in config_names
        assert "example_test_case_3" in config_names
        assert "other_test_case_2" not in config_names

    def test_log_overview_download_section_excludes_skipped_paths(self) -> None:
        # Arrange
        settings = TestBenchSettings()
        settings.command_line_settings.skip_download = [PathType.INPUT]

        logger = MagicMock(spec=ILogger)

        # Act
        settings.log_overview(logger)

        # Assert
        logger.info.assert_any_call("Download : [dependency, references]")
