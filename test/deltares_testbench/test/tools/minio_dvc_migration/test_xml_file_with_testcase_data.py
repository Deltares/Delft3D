"""Unit tests for XML file with testcase data migration."""

import re
import textwrap
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from src.config.dependency import Dependency
from src.config.test_case_path import TestCasePath
from test.helpers.xml_config_helper import make_test_case_config_xml
from tools.minio_dvc_migration.migrate_xmls import extract_data_from_xml_files
from tools.minio_dvc_migration.s3_url_info import S3UrlInfo
from tools.minio_dvc_migration.testcase_data import DependencyKey, TestCaseData
from tools.minio_dvc_migration.xml_file_with_testcase_data import (
    XmlFileWithTestCaseData,
    apply_dependency_version_map,
    build_dependency_version_map,
    filter_cases_to_migrate,
)


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


def test_migration_updates_dependency_version_to_dvc(tmp_path: Path) -> None:
    """Test that XML migration sets dependency version to DVC and renames path."""
    # Arrange
    version = "2024-05-07T13:35:00"
    dep = Dependency(local_dir="e05_f03_zlayers_hydro", case_path="e05_part/f03_zlayers/hydro", version=version)
    xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("e05_part/f03_zlayers/c01_tracer", version=version),
        dependency=dep,
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file = tmp_path / "test_dep.xml"
    temp_file.write_bytes(xml_content.read())

    xml_data = XmlFileWithTestCaseData(temp_file, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    content = temp_file.read_text(encoding="utf-8")
    assert f'version="{version}"' not in content
    assert 'version="DVC"' in content
    assert ">e05_part/f03_zlayers/hydro</dependency>" in content


def test_migration_preserves_non_parseable_dependency_version(tmp_path: Path) -> None:
    """Ensure dependency with non-timestamp version is not rewritten."""
    # Arrange
    dep = Dependency(local_dir="some_dir", case_path="some/path", version="NO VERSION")
    xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case", version="NO VERSION"),
        dependency=dep,
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file = tmp_path / "test_dep_no_version.xml"
    temp_file.write_bytes(xml_content.read())

    xml_data = XmlFileWithTestCaseData(temp_file, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    content = temp_file.read_text(encoding="utf-8")
    assert 'version="NO VERSION"' in content
    assert 'version="DVC"' not in content


def test_migration_updates_dependency_in_included_xml(tmp_path: Path) -> None:
    """Test that dependency migration is applied recursively in xi:included files."""
    # Arrange
    version = "2024-02-08T08:10:00"
    dep = Dependency(local_dir="dep_dir", case_path="e05_part/f03_zlayers/hydro_v2", version=version)

    included_content = make_test_case_config_xml(
        test_case_path=TestCasePath("included/case", version=version),
        dependency=dep,
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )
    included_file = tmp_path / "included_dep.xml"
    included_file.write_bytes(included_content.read())

    main_content = make_test_case_config_xml(
        test_case_path=TestCasePath("main/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
        include='<xi:include href="included_dep.xml"/>',
    )
    main_file = tmp_path / "main_dep.xml"
    main_file.write_bytes(main_content.read())

    xml_data = XmlFileWithTestCaseData(main_file, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert — included file should also have the dependency updated
    included_text = included_file.read_text(encoding="utf-8")
    assert 'version="DVC"' in included_text
    assert ">e05_part/f03_zlayers/hydro_v2</dependency>" in included_text


def test_download_deduplicates_dependencies() -> None:
    """Validate download_from_minio_in_new_folder_structure downloads each dependency only once."""
    # Arrange
    shared_dep_s3_path = "cases/e05_part/f03_zlayers/hydro"
    tc1 = TestCaseData(
        name="tc1",
        version="2024-05-07T13:35:00",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/c01_tracer"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/lnx64/e05_part/f03_zlayers/c01_tracer"),
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    tc2 = TestCaseData(
        name="tc2",
        version="2024-05-07T13:35:00",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/c02_tracer"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/lnx64/e05_part/f03_zlayers/c02_tracer"),
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    xml_data = XmlFileWithTestCaseData(xml_file=Path("test.xml"), testcases=[tc1, tc2])
    rewinder = MagicMock()

    # Act
    xml_data.download_from_minio_in_new_folder_structure(rewinder)

    # Assert — dependency downloaded once, testcases downloaded twice (case + reference each)
    dep_download_calls = [c for c in rewinder.download.call_args_list if c[0][1] == shared_dep_s3_path]
    assert len(dep_download_calls) == 1
    assert rewinder.download.call_count == 5  # 1 dep + 2 testcases * 2 (case + ref)


def test_add_to_dvc_deduplicates_dependencies() -> None:
    """Validate add_to_dvc adds each shared dependency to DVC only once."""
    # Arrange
    shared_dep_s3_path = "cases/e05_part/f03_zlayers/hydro"
    tc1 = TestCaseData(
        name="tc1",
        version="2024-05-07T13:35:00",
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    tc2 = TestCaseData(
        name="tc2",
        version="2024-05-07T13:35:00",
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    xml_data = XmlFileWithTestCaseData(xml_file=Path("test.xml"), testcases=[tc1, tc2])
    repo = MagicMock()

    mock_dep_add = MagicMock(return_value=[Path("dep.dvc")])
    mock_tc_add = MagicMock(return_value=[Path("tc.dvc")])

    # Act
    with (
        patch.object(tc1, "add_dependency_to_dvc", mock_dep_add),
        patch.object(tc2, "add_dependency_to_dvc") as mock_dep_add_tc2,
        patch.object(tc1, "add_to_dvc", mock_tc_add),
        patch.object(tc2, "add_to_dvc", mock_tc_add),
    ):
        result = xml_data.add_to_dvc(repo)

    # Assert — dependency added only once (via tc1), not via tc2
    mock_dep_add.assert_called_once_with(repo=repo)
    mock_dep_add_tc2.assert_not_called()
    assert Path("dep.dvc") in result


@pytest.mark.filterwarnings("ignore::pytest.PytestUnraisableExceptionWarning")
def test_extract_testcase_data_populates_dependency(tmp_path: Path) -> None:
    """Validate extract_data_from_xml_files populates dependency fields."""
    # Arrange
    version = "2024-05-07T13:35:00"
    dep = Dependency(local_dir="e05_f03_zlayers_hydro", case_path="e05_part/f03_zlayers/hydro", version=version)
    xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("e05_part/f03_zlayers/c01_tracer", version=version),
        dependency=dep,
    )

    temp_file = tmp_path / "dep_config.xml"
    temp_file.write_bytes(xml_content.read())

    # Act
    result = extract_data_from_xml_files([temp_file])

    # Assert
    assert len(result) == 1
    tc = result[0].testcases[0]
    assert tc.has_dependency is True
    assert tc.dependency_s3_path == "cases/e05_part/f03_zlayers/hydro"
    assert tc.dependency_version == version
    assert tc.dependency.path == "cases/e05_part/f03_zlayers/hydro"


@pytest.mark.filterwarnings("ignore::pytest.PytestUnraisableExceptionWarning")
def test_extract_testcase_data_no_dependency(tmp_path: Path) -> None:
    """Validate extract_data_from_xml_files leaves dependency empty when no dependency in XML."""
    # Arrange
    version = "2024-06-12T11:45:00"
    xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("e05_part/f01_tracer/c100", version=version),
    )

    temp_file = tmp_path / "no_dep_config.xml"
    temp_file.write_bytes(xml_content.read())

    # Act
    result = extract_data_from_xml_files([temp_file])

    # Assert
    assert len(result) == 1
    tc = result[0].testcases[0]
    assert tc.has_dependency is False
    assert tc.dependency_s3_path == ""


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


def _make_tc_with_dep(
    name: str,
    version: str,
    dep_s3_path: str,
    dep_version: str,
    dep_local_path: str = "",
) -> TestCaseData:
    """Helper to build a TestCaseData with a dependency."""
    return TestCaseData(
        name=name,
        version=version,
        case=S3UrlInfo(hostname="h", bucket="b", path=f"cases/{name}"),
        reference=S3UrlInfo(hostname="h", bucket="b", path=f"references/lnx64/{name}"),
        dependency=S3UrlInfo(hostname="h", bucket="b", path=dep_local_path or f"cases/{dep_s3_path}"),
        dependency_s3_path=f"cases/{dep_s3_path}",
        dependency_version=dep_version,
    )


def test_build_dependency_version_map_no_conflicts() -> None:
    """When all XMLs use the same version for a dependency path, the map is empty."""
    tc1 = _make_tc_with_dep("tc1", "2024-05-07T13:35:00", "e05/hydro", "2024-05-07T13:35:00")
    tc2 = _make_tc_with_dep("tc2", "2024-05-07T13:35:00", "e05/hydro", "2024-05-07T13:35:00")
    xmls = [
        XmlFileWithTestCaseData(Path("a.xml"), [tc1]),
        XmlFileWithTestCaseData(Path("b.xml"), [tc2]),
    ]

    result = build_dependency_version_map(xmls)

    assert result == {}


def test_build_dependency_version_map_with_conflicts() -> None:
    """When the same dependency path has different versions across XMLs, a suffix map is produced."""
    tc1 = _make_tc_with_dep("tc1", "v1", "e05/hydro", "2024-02-08T08:10:00")
    tc2 = _make_tc_with_dep("tc2", "v2", "e05/hydro", "2024-05-07T13:35:00")
    xmls = [
        XmlFileWithTestCaseData(Path("a.xml"), [tc1]),
        XmlFileWithTestCaseData(Path("b.xml"), [tc2]),
    ]

    result = build_dependency_version_map(xmls)

    assert DependencyKey("cases/e05/hydro", "2024-02-08T08:10:00") in result
    assert DependencyKey("cases/e05/hydro", "2024-05-07T13:35:00") in result
    suffix_values = set(result.values())
    assert len(suffix_values) == 2
    assert all(s.startswith("_v") for s in suffix_values)


def test_build_dependency_version_map_assigns_ordered_suffixes() -> None:
    """Versions are sorted chronologically and assigned _v1, _v2, etc."""
    tc1 = _make_tc_with_dep("tc1", "v1", "e05/hydro", "2024-05-07T13:35:00")
    tc2 = _make_tc_with_dep("tc2", "v2", "e05/hydro", "2024-02-08T08:10:00")
    tc3 = _make_tc_with_dep("tc3", "v3", "e05/hydro", "2025-01-01T00:00:00")
    xmls = [
        XmlFileWithTestCaseData(Path("a.xml"), [tc1, tc2]),
        XmlFileWithTestCaseData(Path("b.xml"), [tc3]),
    ]

    result = build_dependency_version_map(xmls)

    assert result[DependencyKey("cases/e05/hydro", "2024-02-08T08:10:00")] == "_v1"
    assert result[DependencyKey("cases/e05/hydro", "2024-05-07T13:35:00")] == "_v2"
    assert result[DependencyKey("cases/e05/hydro", "2025-01-01T00:00:00")] == "_v3"


def test_apply_dependency_version_map_updates_dependency_path() -> None:
    """apply_dependency_version_map renames dependency.path with the version suffix."""
    tc = _make_tc_with_dep(
        "tc1",
        "v1",
        "e05/f03/hydro",
        "2024-02-08T08:10:00",
        dep_local_path="cases/e05/f03/hydro",
    )
    version_map = {DependencyKey("cases/e05/f03/hydro", "2024-02-08T08:10:00"): "_v1"}
    xmls = [XmlFileWithTestCaseData(Path("a.xml"), [tc])]

    apply_dependency_version_map(xmls, version_map)

    assert tc.dependency.path == "cases/e05/f03/hydro_v1"


def test_apply_dependency_version_map_skips_unaffected() -> None:
    """Dependencies not in the version map are left unchanged."""
    tc = _make_tc_with_dep(
        "tc1",
        "v1",
        "e05/f03/hydro",
        "2024-02-08T08:10:00",
        dep_local_path="cases/e05/f03/hydro",
    )
    original_path = tc.dependency.path
    xmls = [XmlFileWithTestCaseData(Path("a.xml"), [tc])]

    apply_dependency_version_map(xmls, {})

    assert tc.dependency.path == original_path


def test_download_deduplicates_by_path_and_version() -> None:
    """Dependencies with the same S3 path but different versions are both downloaded."""
    shared_dep_s3_path = "cases/e05_part/f03_zlayers/hydro"
    tc1 = TestCaseData(
        name="tc1",
        version="2024-02-08T08:10:00",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/tc1"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/lnx64/tc1"),
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro_v1"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-02-08T08:10:00",
    )
    tc2 = TestCaseData(
        name="tc2",
        version="2024-05-07T13:35:00",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/tc2"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/lnx64/tc2"),
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro_v2"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    xml_data = XmlFileWithTestCaseData(xml_file=Path("test.xml"), testcases=[tc1, tc2])
    rewinder = MagicMock()

    xml_data.download_from_minio_in_new_folder_structure(rewinder)

    dep_download_calls = [c for c in rewinder.download.call_args_list if c[0][1] == shared_dep_s3_path]
    assert len(dep_download_calls) == 2
    assert rewinder.download.call_count == 6  # 2 deps + 2 testcases * 2 (case + ref)


def test_add_to_dvc_deduplicates_by_path_and_version() -> None:
    """Dependencies with the same S3 path but different versions are both added to DVC."""
    shared_dep_s3_path = "cases/e05_part/f03_zlayers/hydro"
    tc1 = TestCaseData(
        name="tc1",
        version="2024-02-08T08:10:00",
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro_v1"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-02-08T08:10:00",
    )
    tc2 = TestCaseData(
        name="tc2",
        version="2024-05-07T13:35:00",
        dependency=S3UrlInfo(hostname="h", bucket="b", path="cases/e05_part/f03_zlayers/hydro_v2"),
        dependency_s3_path=shared_dep_s3_path,
        dependency_version="2024-05-07T13:35:00",
    )
    xml_data = XmlFileWithTestCaseData(xml_file=Path("test.xml"), testcases=[tc1, tc2])
    repo = MagicMock()

    mock_dep_add_tc1 = MagicMock(return_value=[Path("dep_v1.dvc")])
    mock_dep_add_tc2 = MagicMock(return_value=[Path("dep_v2.dvc")])
    mock_tc_add = MagicMock(return_value=[Path("tc.dvc")])

    with (
        patch.object(tc1, "add_dependency_to_dvc", mock_dep_add_tc1),
        patch.object(tc2, "add_dependency_to_dvc", mock_dep_add_tc2),
        patch.object(tc1, "add_to_dvc", mock_tc_add),
        patch.object(tc2, "add_to_dvc", mock_tc_add),
    ):
        result = xml_data.add_to_dvc(repo)

    mock_dep_add_tc1.assert_called_once_with(repo=repo)
    mock_dep_add_tc2.assert_called_once_with(repo=repo)
    assert Path("dep_v1.dvc") in result
    assert Path("dep_v2.dvc") in result


def test_migration_updates_dependency_path_with_version_suffix(tmp_path: Path) -> None:
    """XML migration applies version suffix to dependency path when version map is provided."""
    # Arrange
    version = "2024-02-08T08:10:00"
    dep = Dependency(local_dir="e05_f03_hydro", case_path="e05_part/f03_zlayers/hydro", version=version)
    xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("e05_part/f03_zlayers/c01_tracer", version=version),
        dependency=dep,
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file = tmp_path / "test_dep_suffix.xml"
    temp_file.write_bytes(xml_content.read())

    version_map = {DependencyKey("cases/e05_part/f03_zlayers/hydro", version): "_v1"}
    xml_data = XmlFileWithTestCaseData(temp_file, [], dependency_version_map=version_map)

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    content = temp_file.read_text(encoding="utf-8")
    assert ">e05_part/f03_zlayers/hydro_v1</dependency>" in content
    assert ">e05_part/f03_zlayers/hydro</dependency>" not in content


# --- verify_and_filter_testcases tests ---


def test_verify_and_filter_keeps_found_testcases() -> None:
    """verify_and_filter_testcases keeps testcases where case and reference are found."""
    tc1 = TestCaseData(
        name="tc1",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/tc1"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/tc1"),
    )
    tc2 = TestCaseData(
        name="tc2",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/tc2"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/tc2"),
    )
    xml_data = XmlFileWithTestCaseData(Path("test.xml"), [tc1, tc2])
    rewinder = MagicMock()
    rewinder.list_objects.return_value = [MagicMock()]

    skipped = xml_data.verify_and_filter_testcases(rewinder)

    assert skipped == []
    assert xml_data.testcases == [tc1, tc2]


def test_verify_and_filter_removes_missing_testcases() -> None:
    """verify_and_filter_testcases removes testcases with missing case or reference."""
    tc_found = TestCaseData(
        name="found",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/found"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/found"),
    )
    tc_missing = TestCaseData(
        name="missing",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/missing"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/missing"),
    )
    xml_data = XmlFileWithTestCaseData(Path("test.xml"), [tc_found, tc_missing])
    rewinder = MagicMock()

    def fake_list_objects(prefix, timestamp=None):
        if "missing" in str(prefix):
            return []
        return [MagicMock()]

    rewinder.list_objects.side_effect = fake_list_objects

    skipped = xml_data.verify_and_filter_testcases(rewinder)

    assert len(skipped) == 1
    assert skipped[0].name == "missing"
    assert len(xml_data.testcases) == 1
    assert xml_data.testcases[0].name == "found"


def test_verify_and_filter_returns_all_when_all_missing() -> None:
    """verify_and_filter_testcases returns all testcases as skipped when all are missing."""
    tc = TestCaseData(
        name="tc1",
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/tc1"),
        reference=S3UrlInfo(hostname="h", bucket="b", path="references/tc1"),
    )
    xml_data = XmlFileWithTestCaseData(Path("test.xml"), [tc])
    rewinder = MagicMock()
    rewinder.list_objects.return_value = []

    skipped = xml_data.verify_and_filter_testcases(rewinder)

    assert len(skipped) == 1
    assert xml_data.testcases == []


def test_migration_updates_standalone_configuration_xml(tmp_path: Path) -> None:
    """Test that migration updates location roots and local paths in a standalone
    configuration.xml where <config> is the root element and no testcases exist."""
    # Arrange
    version = "2025-01-15T10:00:00"

    # Create a standalone configuration.xml with <config> as root (no testcases)
    configuration_xml = textwrap.dedent(
        """\
        <config xmlns="http://schemas.deltares.nl/deltaresTestbench_v3">
          <localPaths>
            <testCasesDir>./data/cases</testCasesDir>
            <enginesDir>./data/engines</enginesDir>
            <referenceDir>./data/reference_results</referenceDir>
          </localPaths>
          <locations>
            <location name="reference_results">
              <credential ref="commandline"/>
              <root>{server_base_url}/references</root>
            </location>
            <location name="cases">
              <credential ref="commandline"/>
              <root>{server_base_url}/cases</root>
            </location>
            <location name="engines">
              <root>./data/engines</root>
            </location>
          </locations>
        </config>
        """
    )
    include_dir = tmp_path / "include"
    include_dir.mkdir()
    config_file = include_dir / "configuration.xml"
    config_file.write_text(configuration_xml, encoding="utf-8")

    # Create a main XML that xi:includes the configuration.xml
    xi_include = '<xi:include href="include/configuration.xml"/>'
    main_xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
        include=xi_include,
    )
    main_file = tmp_path / "main_config.xml"
    main_file.write_bytes(main_xml_content.read())

    xml_data = XmlFileWithTestCaseData(main_file, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert - configuration.xml location roots should be updated
    config_content = config_file.read_text(encoding="utf-8")

    assert get_location_root(config_content, "cases") == "./data/cases"
    assert get_location_root(config_content, "reference_results") == "./data/cases"
    assert get_location_root(config_content, "engines") == "./data/engines"
    assert "{server_base_url}" not in config_content

    # Assert - referenceDir in localPaths should also be updated
    assert ">./data/reference_results<" not in config_content
    assert "<referenceDir>./data/cases</referenceDir>" in config_content
