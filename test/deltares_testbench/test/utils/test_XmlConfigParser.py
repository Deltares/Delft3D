import tempfile
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterator, Optional
from unittest.mock import MagicMock, patch

import pytest
from lxml import etree

from src.config.credentials import Credentials
from src.config.dependency import Dependency
from src.config.test_case_path import TestCasePath
from src.suite.command_line_settings import CommandLineSettings
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.test_result_type import TestResultType
from src.utils.xml_config_parser import XmlConfigParser
from test.helpers.xml_config_helper import make_test_case_config_xml


@pytest.fixture()
def tmp_dir() -> Iterator[Path]:
    """Create temporary directory that is cleaned up after tests."""
    with tempfile.TemporaryDirectory() as tmp_dir_name:
        tmp_dir = Path(tmp_dir_name)
        yield tmp_dir


class TestXmlConfigParser:
    def test_load__config_with_testcase__path_not_versioned(self) -> None:
        """It should parse a simple testcase with non-versioned path."""
        # Arrange
        content = make_test_case_config_xml(test_case_path=TestCasePath("test/case/path"))
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.server_base_url = "s3://dsc-testbench"
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)

        # Act
        xml_config = parser.load(settings, logger)

        # Assert
        test_config = xml_config.testcase_configs[0]
        assert len(xml_config.testcase_configs) == 1
        assert test_config.path is not None
        assert test_config.path.path == "test/case/path"
        assert test_config.path.version is None

    def test_load__config_with_testcase__path_versioned(self) -> None:
        """It should parse a simple testcase with versioned path."""
        # Arrange
        now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
        version = now.isoformat().split("+", 1)[0]
        content = make_test_case_config_xml(
            test_case_path=TestCasePath("test/case/path", version),
        )
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.server_base_url = "s3://dsc-testbench"
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)

        # Act
        xml_config = parser.load(settings, logger)

        # Assert
        test_config = xml_config.testcase_configs[0]
        assert len(xml_config.testcase_configs) == 1
        assert test_config.path is not None
        assert test_config.path.path == "test/case/path"
        assert test_config.path.version == version
        assert datetime.fromisoformat(xml_config.testcase_configs[0].path.version).replace(tzinfo=timezone.utc) == now

    def test_load__config_with_testcase_dependency__dependency_not_versioned(self) -> None:
        """It should parse a simple testcase with non-versioned dependency."""
        # Arrange
        content = make_test_case_config_xml(
            dependency=Dependency(local_dir="local/dir", case_path="case/dir"),
        )
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.server_base_url = "s3://dsc-testbench"
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)

        # Act
        xml_config = parser.load(settings, logger)

        # Assert
        test_config = xml_config.testcase_configs[0]
        assert len(xml_config.testcase_configs) == 1
        assert test_config.path is not None
        assert test_config.dependency is not None
        assert test_config.dependency.local_dir == "local/dir"
        assert test_config.dependency.version is None

    def test_load_with_minio_path(self) -> None:
        """It should parse a simple testcase with non-versioned dependency."""
        # Arrange
        now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
        version = now.isoformat().split("+", 1)[0]
        content = make_test_case_config_xml(
            test_case_path=TestCasePath("test/case/path", version),
        )
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.server_base_url = "https://abcdefg"
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)

        # Act
        xml_config = parser.load(settings, logger)

        # Assert
        test_config = xml_config.testcase_configs[0]
        assert len(xml_config.testcase_configs) == 1
        assert test_config.path is not None
        assert test_config.path.prefix == "test/case/path"
        assert len(test_config.locations) == 2
        assert test_config.locations[0].name == "dsctestbench-cases"
        assert test_config.locations[0].from_path == "."
        assert test_config.locations[0].root == "https://abcdefg/cases"
        assert test_config.locations[1].name == "dsctestbench-references"
        assert test_config.locations[1].from_path == "win64"
        assert test_config.locations[1].root == "https://abcdefg/references"

    def test_load_with_local_dvc_path(self) -> None:
        """It should parse a simple testcase with non-versioned dependency."""
        # Arrange
        content = make_test_case_config_xml(
            test_case_path=TestCasePath("e02_dflowfm/f012_inout/c0322_alloutrealistic_f12_e02_3dom", version="DVC"),
            case_root="data/cases/",
            reference_root="data/cases/",
        )
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)

        # Act
        xml_config = parser.load(settings, logger)

        # Assert
        test_config = xml_config.testcase_configs[0]
        assert len(xml_config.testcase_configs) == 1
        assert test_config.path is not None
        assert test_config.path.prefix == "e02_dflowfm/f012_inout/c0322_alloutrealistic_f12_e02_3dom"
        assert test_config.path.version == "DVC"
        assert len(test_config.locations) == 2
        assert test_config.locations[0].name == "dsctestbench-cases"
        assert test_config.locations[0].from_path == "."
        assert test_config.locations[0].root == "data/cases/"
        assert test_config.locations[1].name == "dsctestbench-references"
        assert test_config.locations[1].from_path == "win64"
        assert test_config.locations[1].root == "data/cases/"

    def test_load__config_with_11e__throws_error_and_logs(self) -> None:
        """Throw and log value error in xml parsing."""
        # Arrange
        content = make_test_case_config_xml(reference_value="11.0e")
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.config_file = content
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"

        # Mock the logger
        logger = MagicMock(spec=ConsoleLogger)
        testcase_logger = MagicMock()
        logger.create_test_case_logger.return_value = testcase_logger

        # Act
        with patch("src.utils.logging.test_loggers.file_test_logger.FileTestLogger", return_value=testcase_logger):
            with pytest.raises(Exception) as excinfo:
                _ = parser.load(settings, logger)

        # Assert
        assert excinfo.typename == "ValueError"
        logger.create_test_case_logger.assert_called_once()
        testcase_logger.test_started.assert_called_once()
        testcase_logger.test_Result.assert_called_once_with(
            TestResultType.Exception, "could not convert string to float: '11.0e'"
        )

    def test_assert_validation_error(self, tmp_dir: Path) -> None:
        # Arrange
        settings = self.setup_include_element_xml(tmp_dir, "vrsion=")
        logger = ConsoleLogger(LogLevel.DEBUG)
        parser = XmlConfigParser()

        # Act & Assert
        with pytest.raises(Exception) as excinfo:
            _ = parser.load(settings, logger)
        assert excinfo.type == etree.DocumentInvalid

    def test_handle_include_and_validate(self, tmp_dir: Path) -> None:
        # Arrange
        settings = self.setup_include_element_xml(tmp_dir)
        logger = ConsoleLogger(LogLevel.DEBUG)
        parser = XmlConfigParser()
        _ = parser.load(settings, logger)

    @pytest.mark.parametrize(
        ("server_base_url", "case_root", "expected_root"),
        [
            ("s3://dsc-testbench", "{server_base_url}/references", "s3://dsc-testbench/references"),
            ("https://example.com/", "{server_base_url}/cases", "https://example.com/cases"),
            ("https://example.com/", "{server_base_url}cases", "https://example.com/cases"),
            ("https://example.com", "{server_base_url}cases", "https://example.com/cases"),
            ("https://example.com", "{server_base_url}/cases", "https://example.com/cases"),
            ("", "{server_base_url}cases", "cases"),
            ("", "{server_base_url}/cases", "cases"),
        ],
    )
    def test_replace_handle_bars(self, server_base_url: str, case_root: str, expected_root: str) -> None:
        # Arrange
        parser = XmlConfigParser()
        settings = CommandLineSettings()
        settings.server_base_url = server_base_url
        content = make_test_case_config_xml(case_root=case_root)
        settings.config_file = content
        settings.credentials = Credentials()
        settings.credentials.name = "commandline"
        logger = ConsoleLogger(LogLevel.DEBUG)
        # Act

        xml_config = parser.load(settings, logger)
        case_location = next(
            loc for loc in xml_config.testcase_configs[0].locations if loc.name == "dsctestbench-cases"
        )

        # Assert
        assert case_location.root == expected_root

    def setup_include_element_xml(self, tmp_dir: Path, version_attr: Optional[str] = "version=") -> CommandLineSettings:
        now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
        version = now.isoformat().split("+", 1)[0]
        include_xml = f"""
        <testCases xmlns="http://schemas.deltares.nl/deltaresTestbench_v3">
            <testCase name="run_foo" ref="default_test_case">
                <path {version_attr}"{version}">local/dir</path>
                <programs>
                    <program ref="foo"></program>
                </programs>
                <maxRunTime>60.0</maxRunTime>
                <checks>
                    <file name="foo.out" type=".out">
                        <parameters>
                            <parameter name="foo" toleranceRelative="0.0" />
                        </parameters>
                    </file>
                </checks>
            </testCase>
        </testCases>
        """
        xml_include_path = tmp_dir / "include.xml"
        with open(xml_include_path, "w", encoding="utf-8") as file:
            file.write(include_xml)
        include = f'<xi:include href="{xml_include_path.as_posix()}"/>'
        settings = CommandLineSettings()
        settings.config_file = make_test_case_config_xml(include=include)
        return settings
