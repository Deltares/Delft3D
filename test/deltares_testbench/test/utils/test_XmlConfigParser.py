import io
import textwrap
from datetime import datetime, timezone
from typing import Optional
from unittest.mock import MagicMock, patch

import pytest

from src.config.credentials import Credentials
from src.config.dependency import Dependency
from src.config.test_case_path import TestCasePath
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.test_result_type import TestResultType
from src.utils.xml_config_parser import XmlConfigParser


def test_load__config_with_testcase__path_not_versioned() -> None:
    """It should parse a simple testcase with non-versioned path."""
    # Arrange
    content = make_test_case_config(test_case_path=TestCasePath("test/case/path"))
    parser = XmlConfigParser()
    settings = TestBenchSettings()
    settings.config_file = content
    settings.server_base_url = "s3://dsc-testbench"
    settings.credentials = Credentials()
    settings.credentials.name = "commandline"
    logger = ConsoleLogger(LogLevel.DEBUG)

    # Act
    _, _, (config, *other_configs) = parser.load(settings, logger)  # type: ignore

    # Assert
    assert not other_configs
    assert config.path is not None
    assert config.path.path == "test/case/path"
    assert config.path.version is None


def test_load__config_with_testcase__path_versioned() -> None:
    """It should parse a simple testcase with versioned path."""
    # Arrange
    now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
    version = now.isoformat().split("+", 1)[0]
    content = make_test_case_config(
        test_case_path=TestCasePath("test/case/path", version),
    )
    parser = XmlConfigParser()
    settings = TestBenchSettings()
    settings.config_file = content
    settings.server_base_url = "s3://dsc-testbench"
    settings.credentials = Credentials()
    settings.credentials.name = "commandline"
    logger = ConsoleLogger(LogLevel.DEBUG)

    # Act
    _, _, (config, *other_configs) = parser.load(settings, logger)  # type: ignore

    # Assert
    assert not other_configs
    assert config.path is not None
    assert config.path.path == "test/case/path"
    assert config.path.version == version
    assert datetime.fromisoformat(config.path.version).replace(tzinfo=timezone.utc) == now


def test_load__config_with_testcase_depencency__dependency_not_versioned() -> None:
    """It should parse a simple testcase with non-versioned dependency."""
    # Arrange
    content = make_test_case_config(
        dependency=Dependency(local_dir="local/dir", case_path="case/dir"),
    )
    parser = XmlConfigParser()
    settings = TestBenchSettings()
    settings.config_file = content
    settings.server_base_url = "s3://dsc-testbench"
    settings.credentials = Credentials()
    settings.credentials.name = "commandline"
    logger = ConsoleLogger(LogLevel.DEBUG)

    # Act
    _, _, (config, *other_configs) = parser.load(settings, logger)  # type: ignore

    # Assert
    assert not other_configs
    assert config.path is not None
    assert config.dependency is not None
    assert config.dependency.local_dir == "local/dir"
    assert config.dependency.version is None


def test_load__config_with_testcase_dependency__dependency_versioned() -> None:
    """It should parse a simple testcase with versioned dependency."""
    # Arrange
    now = datetime.now(timezone.utc).replace(second=0, microsecond=0)
    version = now.isoformat().split("+", 1)[0]
    content = make_test_case_config(
        dependency=Dependency(local_dir="local/dir", case_path="case/dir", version=version),
    )
    parser = XmlConfigParser()
    settings = TestBenchSettings()
    settings.config_file = content
    settings.server_base_url = "s3://dsc-testbench"
    settings.credentials = Credentials()
    settings.credentials.name = "commandline"
    logger = ConsoleLogger(LogLevel.DEBUG)

    # Act
    _, _, (config, *other_configs) = parser.load(settings, logger)  # type: ignore

    # Assert
    assert not other_configs
    assert config.path is not None
    assert config.dependency is not None
    assert config.dependency.local_dir == "local/dir"
    assert config.dependency.version == version
    assert datetime.fromisoformat(config.dependency.version).replace(tzinfo=timezone.utc) == now


def test_load__config_with_11e__throws_error_and_logs() -> None:
    """Throw and log value error in xml parsing."""
    # Arrange
    content = make_test_case_config(reference_value="11.0e")
    parser = XmlConfigParser()
    settings = TestBenchSettings()
    settings.config_file = content
    settings.credentials = Credentials()
    settings.credentials.name = "commandline"

    # Mock the logger
    logger = MagicMock(spec=ConsoleLogger)
    testcase_logger = MagicMock()
    logger.create_test_case_logger.return_value = testcase_logger

    # Act
    with patch('src.utils.logging.test_loggers.file_test_logger.FileTestLogger', return_value=testcase_logger):
        with pytest.raises(Exception) as excinfo:
            _, _, _ = parser.load(settings, logger)

    # Assert
    assert excinfo.typename == 'ValueError'
    logger.create_test_case_logger.assert_called_once()
    testcase_logger.test_started.assert_called_once()
    testcase_logger.test_Result.assert_called_once_with(TestResultType.Exception, "could not convert string to float: '11.0e'")


def make_test_case_config(
    test_case_path: Optional[TestCasePath] = None,
    dependency: Optional[Dependency] = None,
    reference_value: Optional[str] = "0.0",
) -> io.BytesIO:
    """Make config xml with some default values."""
    # Build `path` element.
    test_case_path = test_case_path or TestCasePath("test/case")
    path_elem = "<path"
    if test_case_path.version is not None:
        path_elem += f' version="{test_case_path.version}"'
    path_elem += f">{test_case_path.path}</path>"

    # Build `dependency` element.
    dependency_elem = ""
    if dependency:
        dependency_elem = f'<dependency local_dir="{dependency.local_dir}"'
        if dependency.version is not None:
            dependency_elem += f' version="{dependency.version}"'
        dependency_elem += f">{dependency.version}</dependency>"

    text = textwrap.dedent(
        rf"""
        <?xml version="1.0" encoding="iso-8859-1"?>
        <deltaresTestbench_v3 xmlns="http://schemas.deltares.nl/deltaresTestbench_v3"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://schemas.deltares.nl/deltaresTestbench_v3 http://content.oss.deltares.nl/schemas/deltaresTestbench_v3-2.00.xsd">
            <config>
                <credentials>
                    <credential name="deltares">
                        <username></username>
                        <password></password>
                    </credential>
                </credentials>
                <localPaths>
                    <testCasesDir>.\data\cases</testCasesDir>
                    <enginesDir>.\data\engines</enginesDir>
                    <referenceDir>.\data\references_results</referenceDir>
                </localPaths>
                <locations>
                    <location name="dsctestbench-cases">
                        <credential ref="deltares" />
                        <root>https://s3.deltares.nl/cases</root>
                    </location>
                    <location name="dsctestbench-references">
                        <credential ref="deltares" />
                        <root>https:/s3.deltares.nl/references</root>
                    </location>
                </locations>
            </config>
            <programs>
                <program name="foo">
                    <path>foo.exe</path>
                </program>
            </programs>
    
            <defaultTestCases>
                <testCase name="default_test_case">
                    <location ref="dsctestbench-cases" type="input">
                        <from>.</from>
                    </location>
                    <location ref="dsctestbench-references" type="reference">
                        <from>.</from>
                    </location>
                    <maxRunTime>60.0</maxRunTime>
                </testCase>
            </defaultTestCases>
            <testCases>
                <testCase name="run_foo" ref="default_test_case">
                    {path_elem}
                    {dependency_elem}
                    <programs>
                        <program ref="foo"></program>
                    </programs>
                    <maxRunTime>60.0</maxRunTime>
                    <checks>
                        <file name="foo.out" type=".out">
                            <parameters>
                                <parameter name="foo" toleranceRelative="{reference_value}" />
                            </parameters>
                        </file>
                    </checks>
                </testCase>
            </testCases>
        </deltaresTestbench_v3>
        """
    )
    return io.BytesIO(text.strip().encode("utf-8"))
