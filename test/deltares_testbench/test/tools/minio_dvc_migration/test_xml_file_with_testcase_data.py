"""Unit tests for XML file with testcase data migration."""

import re
from datetime import datetime, timezone
from pathlib import Path

import pytest

from src.config.test_case_path import TestCasePath
from test.helpers.xml_config_helper import make_test_case_config_xml
from tools.minio_dvc_migration.migrate_xmls import extract_data_from_xml_files
from tools.minio_dvc_migration.testcase_data import TestCaseData
from tools.minio_dvc_migration.xml_file_with_testcase_data import XmlFileWithTestCaseData, filter_cases_to_migrate


def test_migration_of_minio_to_dvc_testcases_xml(tmp_path: Path) -> None:
    """Test that XML file migration updates paths correctly."""
    # Arrange
    version = datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")
    xml_content_stream = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case/path", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file_path = tmp_path / "test_config.xml"
    with open(temp_file_path, "wb") as temp_file:
        temp_file.write(xml_content_stream.read())

    xml_data = XmlFileWithTestCaseData(temp_file_path, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    with open(temp_file_path, "r", encoding="utf-8") as f:
        modified_content = f.read()

    assert "./data/cases" in modified_content
    assert "{server_base_url}/cases" not in modified_content
    assert "{server_base_url}/references" not in modified_content
    assert 'version="DVC"' in modified_content
    assert f'version="{version}"' not in modified_content


def test_migrate_xml_to_dvc_missing_file(tmp_path: Path) -> None:
    """Ensure migrate_xml_to_dvc raises when the XML file is absent."""
    # Arrange
    missing_xml = tmp_path / "absent.xml"
    xml_data = XmlFileWithTestCaseData(missing_xml, [])

    # Act & Assert
    with pytest.raises(FileNotFoundError, match="XML file does not exist"):
        xml_data.migrate_xml_to_dvc()


def test_migration_of_minio_to_dvc_testcases_xml_with_included_xml(tmp_path: Path) -> None:
    """Test that XML file migration with xi:include updates both main and included files correctly."""
    # Arrange
    version = datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")

    # Create the included XML file
    included_xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("included/test/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    included_file_path = tmp_path / "included_config.xml"
    with open(included_file_path, "wb") as included_file:
        included_file.write(included_xml_content.read())

    # Create the main XML file that includes the other file
    xi_include = '<xi:include href="included_config.xml"/>'
    main_xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("main/test/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
        include=xi_include,
    )

    main_file_path = tmp_path / "main_config.xml"
    with open(main_file_path, "wb") as main_file:
        main_file.write(main_xml_content.read())

    xml_data = XmlFileWithTestCaseData(main_file_path, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert - Read the main file and check changes
    with open(main_file_path, "r", encoding="utf-8") as f:
        main_modified_content = f.read()

    assert "./data/cases" in main_modified_content
    assert "{server_base_url}/cases" not in main_modified_content
    assert "{server_base_url}/references" not in main_modified_content
    assert 'version="DVC"' in main_modified_content
    assert f'version="{version}"' not in main_modified_content

    # Assert - Read the included file and check changes
    with open(included_file_path, "r", encoding="utf-8") as f:
        included_modified_content = f.read()

    assert "./data/cases" in included_modified_content
    assert "{server_base_url}/cases" not in included_modified_content
    assert "{server_base_url}/references" not in included_modified_content
    assert 'version="DVC"' in included_modified_content
    assert f'version="{version}"' not in included_modified_content


def test_migration_preserves_non_parseable_version(tmp_path: Path) -> None:
    """Ensure non-rewind versions are not rewritten to DVC."""
    # Arrange
    version = "NO VERSION"
    xml_content_stream = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case/path", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file_path = tmp_path / "test_config_invalid_version.xml"
    with open(temp_file_path, "wb") as temp_file:
        temp_file.write(xml_content_stream.read())

    xml_data = XmlFileWithTestCaseData(temp_file_path, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    modified_content = temp_file_path.read_text(encoding="utf-8")
    assert f'version="{version}"' in modified_content
    assert 'version="DVC"' not in modified_content


def _tc(version: str) -> TestCaseData:
    return TestCaseData(name="tc", version=version)


def test_filter_cases_to_migrate_filters_by_dvc_and_valid_version() -> None:
    xml_a = XmlFileWithTestCaseData(
        xml_file=Path("a.xml"),
        testcases=[
            _tc("dvc"),
            _tc("DVC"),
            _tc("  dVc  "),
            _tc(""),
            _tc("123"),
            _tc("NO VERSION"),
            _tc("2025-09-11T13:20:21"),
            _tc("2025-09-11T13:20:21.667000"),
            _tc("2025-11-19T10:31"),
        ],
    )
    xml_b = XmlFileWithTestCaseData(xml_file=Path("b.xml"), testcases=[_tc("DVC")])
    xml_c = XmlFileWithTestCaseData(xml_file=Path("c.xml"), testcases=[_tc("NO VERSION"), _tc("not-a-date")])

    result = filter_cases_to_migrate([xml_a, xml_b, xml_c])

    assert [x.xml_file for x in result] == [Path("a.xml")]
    assert [tc.version for tc in result[0].testcases] == [
        "2025-09-11T13:20:21",
        "2025-09-11T13:20:21.667000",
        "2025-11-19T10:31",
    ]


def test_filter_cases_to_migrate_returns_empty_when_none_migratable() -> None:
    xml = XmlFileWithTestCaseData(xml_file=Path("only_dvc.xml"), testcases=[_tc("dvc"), _tc("DVC")])

    result = filter_cases_to_migrate([xml])

    assert result == []


def test_migration_of_locations_testcases_xml(tmp_path: Path) -> None:
    """Test that XML file migration updates paths correctly."""
    # Arrange
    version = datetime.now(timezone.utc).replace(tzinfo=None).isoformat(timespec="microseconds")

    xml_content_stream = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case/path", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
        additional_locations=(
            """
                    <location name="unrelated_location">
                        <credential ref="commandline"/>
                        <root>{server_base_url}/references</root>
                    </location>
            """
        ),
    )

    temp_file_path = tmp_path / "test_config.xml"
    with open(temp_file_path, "wb") as temp_file:
        temp_file.write(xml_content_stream.read())

    xml_files_with_all_testcases = extract_data_from_xml_files([temp_file_path])
    xml_data = xml_files_with_all_testcases[0]

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    modified_content = temp_file_path.read_text(encoding="utf-8")

    assert get_location_root(modified_content, "dsctestbench-cases") == "./data/cases"
    assert get_location_root(modified_content, "dsctestbench-references") == "./data/cases"
    assert get_location_root(modified_content, "unrelated_location") == "{server_base_url}/references"


def get_location_root(content: str, location_name: str) -> str:
    match = re.search(
        rf'<location\s+name="{re.escape(location_name)}"\s*>.*?<root>(.*?)</root>',
        content,
        flags=re.DOTALL,
    )
    if match is not None:
        return (match.group(1) or "").strip()
    else:
        return ""
