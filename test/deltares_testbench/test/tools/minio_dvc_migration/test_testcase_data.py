"""Unit tests for XML parser functions."""

from dataclasses import dataclass
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from tools.minio_dvc_migration.s3_url_info import S3UrlInfo
from tools.minio_dvc_migration.testcase_data import (
    TestCaseData,
    _check_s3_prefix,
    is_case_with_doc_folder,
    rename_dependency_path_for_dvc,
)


def __make_testcase() -> TestCaseData:
    """Create a TestCaseData with fake filesystem directories."""
    testcase = TestCaseData(
        xml_file="config.xml",
        name="example",
        case=S3UrlInfo(
            hostname="example.org", bucket="bucket", path="cases/E07_sobek/F61_rws_acceptance/C13_maas_14_js4"
        ),
        reference=S3UrlInfo(
            hostname="example.org",
            bucket="bucket",
            path="references/lnx64/E07_sobek/F61_rws_acceptance/C13_maas_14_js4",
        ),
    )
    return testcase


@dataclass(frozen=True)
class _TestCasePaths:
    """Paths created for a test case in the fake filesystem."""

    case: Path
    reference: Path
    doc: Path


def __make_testcase_paths(fs: FakeFilesystem, testcase: TestCaseData) -> _TestCasePaths:
    case_path = testcase.case.to_local()
    reference_path = testcase.reference.to_local()
    doc_path = case_path.parent / "doc"

    fs.create_dir(case_path)
    fs.create_dir(reference_path)
    fs.create_dir(doc_path)

    return _TestCasePaths(case=case_path, reference=reference_path, doc=doc_path)


def _fake_add_directory_factory(responses: dict[Path, list[Path]]) -> MagicMock:
    """Build a fake add_directory_to_dvc callable backed by provided responses."""

    def _fake_add_directory(target_path: Path, _repo: MagicMock) -> list[Path]:
        if not target_path.exists():
            return []
        return responses.get(target_path, [])

    return MagicMock(side_effect=_fake_add_directory)


@pytest.mark.parametrize(
    ("case_path"),
    [
        Path("data/cases/E07_sobek/F61_rws_acceptance/C13_maas_14_js4/input"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
        Path("data/cases/e02_dflowfm/f010_structures/c102_custompoints_dambreak/T1_normal_noCustomPoints/input"),
        Path("data/cases/e02_dflowfm/f107_1d2d_validation/c01_1d-2d-urban/T1/input"),
        Path("data/cases/e02_dflowfm/f151_1d2d_acceptance_rural/c11_dar-es-salaam/dflowfm/testmodel/input"),
    ],
)
def test_is_case_folder_valid_pattern_with_doc(fs: FakeFilesystem, case_path: Path) -> None:
    """Test is_case_folder returns True for valid pattern with doc folder."""
    # Arrange
    fs.create_dir(case_path)

    doc_path = case_path / "doc"
    fs.create_dir(doc_path)

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is True


def test_is_case_folder_valid_pattern_without_doc(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for valid pattern without doc folder."""
    # Arrange
    case_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")
    fs.create_dir(case_path)

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is False


def test_is_case_folder_doc_is_file_not_directory(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False when 'doc' exists but is a file, not directory."""
    # Arrange
    case_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")
    fs.create_dir(case_path)

    doc_path = case_path / "doc"
    fs.create_file(doc_path, contents="This is a doc file")

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is False


def test_is_case_folder_invalid_pattern(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for paths that don't match the pattern."""
    invalid_paths = [
        Path("data/cases/invalid/path"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/output"),
        Path("data/cases/e07_sobek/invalid"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/13c_maas_14_js4/input"),
        Path("other/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
        Path("data/references/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
    ]

    # Arrange
    for path in invalid_paths:
        fs.create_dir(path)
        fs.create_dir(path / "doc")

    # Act & Assert
    for path in invalid_paths:
        result = is_case_with_doc_folder(path)
        assert result is False, f"Expected False for {path}"


def test_is_case_folder_nonexistent_directory(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for non-existent directories."""
    # Arrange
    nonexistent_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")

    # Act
    result = is_case_with_doc_folder(nonexistent_path)

    # Assert
    assert result is False


@pytest.mark.parametrize(
    ("fail_target", "expected_message"),
    [
        ("case", "Failed to add case to DVC"),
        ("reference", "Failed to add reference to DVC"),
        ("doc", "Failed to add doc folder to DVC"),
    ],
)
def test_add_to_dvc_failing(fs: FakeFilesystem, fail_target: str, expected_message: str) -> None:
    """Validate add_to_dvc raises when one of the paths fails."""
    # Arrange
    testcase = __make_testcase()
    paths = __make_testcase_paths(fs, testcase)
    repo = MagicMock()

    responses = {
        paths.case: [Path("case.dvc")],
        paths.reference: [Path("reference.dvc")],
        paths.doc: [Path("doc.dvc")],
    }

    failure_map = {
        "case": paths.case,
        "reference": paths.reference,
        "doc": paths.doc,
    }
    responses[failure_map[fail_target]] = []

    # Act & Assert
    fake_add_directory = _fake_add_directory_factory(responses)
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        with pytest.raises(RuntimeError, match=expected_message):
            testcase.add_to_dvc(repo)


def test_add_to_dvc_missing_doc_folder(fs: FakeFilesystem) -> None:
    """Validate add_to_dvc skips doc folder when it does not exist."""
    # Arrange
    testcase = __make_testcase()
    case_path = testcase.case.to_local()
    reference_path = testcase.reference.to_local()

    fs.create_dir(case_path)
    fs.create_dir(reference_path)

    repo = MagicMock()

    responses = {
        case_path: [Path("case.dvc")],
        reference_path: [Path("reference.dvc")],
    }

    # Act
    fake_add_directory = _fake_add_directory_factory(responses)
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        result = testcase.add_to_dvc(repo)

    # Assert — only case and reference, no doc
    assert [path.name for path in result] == ["case.dvc", "reference.dvc"]


def test_add_to_dvc_success(fs: FakeFilesystem) -> None:
    """Validate add_to_dvc returns all DVC files when successful."""
    # Arrange
    testcase = __make_testcase()
    paths = __make_testcase_paths(fs, testcase)
    repo = MagicMock()

    responses = {
        paths.case: [Path("case.dvc")],
        paths.reference: [Path("reference.dvc")],
        paths.doc: [Path("doc.dvc")],
    }

    # Act
    fake_add_directory = _fake_add_directory_factory(responses)
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        result = testcase.add_to_dvc(repo)

    # Assert
    assert [path.name for path in result] == ["case.dvc", "reference.dvc", "doc.dvc"]


@pytest.mark.parametrize(
    ("input_path", "expected"),
    [
        ("e05_part/f03_zlayers/hydro", "e05_part/f03_zlayers/hydro"),
        ("e05_part/f03_zlayers/hydro_v2", "e05_part/f03_zlayers/hydro_v2"),
        ("e05_part/f08_leeway/hydro", "e05_part/f08_leeway/hydro"),
        ("e05_part/f08_leeway_zlayers/hydro", "e05_part/f08_leeway_zlayers/hydro"),
        ("some/path/compute_data", "some/path/compute_data"),
        ("e05_part/f03_zlayers/c00_hydro", "e05_part/f03_zlayers/c00_hydro"),
    ],
)
def test_rename_dependency_path_preserves_path(input_path: str, expected: str) -> None:
    """Validate rename_dependency_path_for_dvc preserves the original path."""
    assert rename_dependency_path_for_dvc(input_path) == expected


@pytest.mark.parametrize(
    ("input_path", "suffix", "expected"),
    [
        ("e05_part/f03_zlayers/hydro", "_v1", "e05_part/f03_zlayers/hydro_v1"),
        ("e05_part/f03_zlayers/hydro", "_v2", "e05_part/f03_zlayers/hydro_v2"),
        ("e05_part/f03_zlayers/c00_hydro", "_v1", "e05_part/f03_zlayers/c00_hydro_v1"),
        ("some/path/data", "", "some/path/data"),
    ],
)
def test_rename_dependency_path_with_version_suffix(input_path: str, suffix: str, expected: str) -> None:
    """Validate rename_dependency_path_for_dvc appends version suffix to the final segment."""
    assert rename_dependency_path_for_dvc(input_path, version_suffix=suffix) == expected


def __make_testcase_with_dependency() -> TestCaseData:
    """Create a TestCaseData with dependency fields populated."""
    testcase = __make_testcase()
    testcase.dependency = S3UrlInfo(
        hostname="example.org",
        bucket="bucket",
        path="cases/e05_part/f03_zlayers/hydro",
    )
    testcase.dependency_s3_path = "cases/e05_part/f03_zlayers/hydro"
    testcase.dependency_version = "2024-05-07T13:35:00"
    return testcase


def test_has_dependency_returns_true_when_set() -> None:
    """Validate has_dependency is True when dependency.path is populated."""
    testcase = __make_testcase_with_dependency()
    assert testcase.has_dependency is True


def test_has_dependency_returns_false_when_empty() -> None:
    """Validate has_dependency is False when dependency.path is empty."""
    testcase = __make_testcase()
    assert testcase.has_dependency is False


def test_download_dependency_calls_rewinder() -> None:
    """Validate download_dependency downloads from the original S3 path to the renamed local path."""
    # Arrange
    testcase = __make_testcase_with_dependency()
    rewinder = MagicMock()

    # Act
    testcase.download_dependency(rewinder)

    # Assert
    rewinder.download.assert_called_once()
    call_args = rewinder.download.call_args
    assert call_args[0][0] == "bucket"
    assert call_args[0][1] == "cases/e05_part/f03_zlayers/hydro"
    assert call_args[0][2] == testcase.dependency.to_local()
    assert call_args[0][3] is not None  # rewind timestamp is parsed


def test_download_dependency_skips_when_no_dependency() -> None:
    """Validate download_dependency does nothing when has_dependency is False."""
    # Arrange
    testcase = __make_testcase()
    rewinder = MagicMock()

    # Act
    testcase.download_dependency(rewinder)

    # Assert
    rewinder.download.assert_not_called()


def test_add_dependency_to_dvc_success(fs: FakeFilesystem) -> None:
    """Validate add_dependency_to_dvc returns DVC files when successful."""
    # Arrange
    testcase = __make_testcase_with_dependency()
    dep_path = testcase.dependency.to_local()
    fs.create_dir(dep_path)
    repo = MagicMock()

    responses = {dep_path: [Path("dependency.dvc")]}
    fake_add_directory = _fake_add_directory_factory(responses)

    # Act
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        result = testcase.add_dependency_to_dvc(repo)

    # Assert
    assert [path.name for path in result] == ["dependency.dvc"]


def test_add_dependency_to_dvc_raises_on_failure(fs: FakeFilesystem) -> None:
    """Validate add_dependency_to_dvc raises RuntimeError when DVC add fails."""
    # Arrange
    testcase = __make_testcase_with_dependency()
    dep_path = testcase.dependency.to_local()
    fs.create_dir(dep_path)
    repo = MagicMock()

    responses = {dep_path: []}
    fake_add_directory = _fake_add_directory_factory(responses)

    # Act & Assert
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        with pytest.raises(RuntimeError, match="Failed to add dependency to DVC"):
            testcase.add_dependency_to_dvc(repo)


def test_add_dependency_to_dvc_returns_empty_when_no_dependency() -> None:
    """Validate add_dependency_to_dvc returns empty list when has_dependency is False."""
    # Arrange
    testcase = __make_testcase()
    repo = MagicMock()

    # Act
    result = testcase.add_dependency_to_dvc(repo)

    # Assert
    assert result == []


# --- _check_s3_prefix tests ---


def test_check_s3_prefix_found() -> None:
    """_check_s3_prefix returns 'found' when objects exist at the prefix."""
    rewinder = MagicMock()
    rewinder.list_objects.return_value = [MagicMock()]

    result = _check_s3_prefix(rewinder, "bucket", "cases/some/path")

    assert result == "found"
    rewinder.list_objects.assert_called_once()


def test_check_s3_prefix_missing_empty_listing() -> None:
    """_check_s3_prefix returns 'missing' when no objects at the prefix."""
    rewinder = MagicMock()
    rewinder.list_objects.return_value = []

    result = _check_s3_prefix(rewinder, "bucket", "cases/some/path")

    assert result == "missing"


def test_check_s3_prefix_missing_empty_path() -> None:
    """_check_s3_prefix returns 'missing' when path is empty."""
    rewinder = MagicMock()

    result = _check_s3_prefix(rewinder, "bucket", "")

    assert result == "missing"
    rewinder.list_objects.assert_not_called()


def test_check_s3_prefix_error_on_exception() -> None:
    """_check_s3_prefix returns 'error' when list_objects raises."""
    rewinder = MagicMock()
    rewinder.list_objects.side_effect = Exception("connection failed")

    result = _check_s3_prefix(rewinder, "bucket", "cases/some/path")

    assert result == "error"


# --- TestCaseData.download tests ---


def test_download_calls_rewinder_for_case_and_reference() -> None:
    """download() calls rewinder.download for both case and reference."""
    testcase = __make_testcase()
    testcase.version = "2024-05-07T13:35:00"
    rewinder = MagicMock()

    testcase.download(rewinder)

    assert rewinder.download.call_count == 2
    call_args_list = rewinder.download.call_args_list
    assert call_args_list[0][0][1] == testcase.case.path
    assert call_args_list[1][0][1] == testcase.reference.path


def test_download_no_version_passes_none_timestamp() -> None:
    """download() passes None as timestamp when version is empty."""
    testcase = __make_testcase()
    testcase.version = ""
    rewinder = MagicMock()

    testcase.download(rewinder)

    assert rewinder.download.call_count == 2
    assert rewinder.download.call_args_list[0][0][3] is None


# --- check_minio_presence tests ---


def test_check_minio_presence_both_found() -> None:
    """check_minio_presence returns 'found' for both when objects exist."""
    testcase = __make_testcase()
    testcase.version = "2024-05-07T13:35:00"
    rewinder = MagicMock()
    rewinder.list_objects.return_value = [MagicMock()]

    result = testcase.check_minio_presence(rewinder)

    assert result["case"] == "found"
    assert result["reference"] == "found"


def test_check_minio_presence_both_missing() -> None:
    """check_minio_presence returns 'missing' when no objects exist."""
    testcase = __make_testcase()
    testcase.version = "2024-05-07T13:35:00"
    rewinder = MagicMock()
    rewinder.list_objects.return_value = []

    result = testcase.check_minio_presence(rewinder)

    assert result["case"] == "missing"
    assert result["reference"] == "missing"


# --- check_doc_folder_minio_presence tests ---


def test_check_doc_folder_minio_presence_found() -> None:
    """check_doc_folder_minio_presence returns 'found' when doc exists on MinIO."""
    testcase = TestCaseData(
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/e01/f01/c01"),
    )
    rewinder = MagicMock()
    rewinder.list_objects.return_value = [MagicMock()]

    result = testcase.check_doc_folder_minio_presence(rewinder)

    assert result == "found"


def test_check_doc_folder_minio_presence_missing() -> None:
    """check_doc_folder_minio_presence returns 'missing' when doc does not exist."""
    testcase = TestCaseData(
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/e01/f01/c01"),
    )
    rewinder = MagicMock()
    rewinder.list_objects.return_value = []

    result = testcase.check_doc_folder_minio_presence(rewinder)

    assert result == "missing"


def test_check_doc_folder_minio_presence_non_matching_path() -> None:
    """check_doc_folder_minio_presence returns None for non eNN/fNN/cNN paths."""
    testcase = TestCaseData(
        case=S3UrlInfo(hostname="h", bucket="b", path="cases/some/other/path"),
    )
    rewinder = MagicMock()

    result = testcase.check_doc_folder_minio_presence(rewinder)

    assert result is None
    rewinder.list_objects.assert_not_called()
