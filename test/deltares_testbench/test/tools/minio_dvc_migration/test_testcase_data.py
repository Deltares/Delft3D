"""Unit tests for XML parser functions."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from tools.minio_dvc_migration.s3_url_info import S3UrlInfo
from tools.minio_dvc_migration.testcase_data import TestCaseData, is_case_with_doc_folder


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


def __make_testcase_paths(fs: FakeFilesystem, testcase: TestCaseData) -> tuple[Path, Path, Path]:
    case_path = testcase.case.to_local()
    reference_path = testcase.reference.to_local()
    doc_path = case_path.parent / "doc"

    fs.create_dir(case_path)
    fs.create_dir(reference_path)
    fs.create_dir(doc_path)

    return case_path, reference_path, doc_path


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
    case_path, reference_path, doc_path = __make_testcase_paths(fs, testcase)
    repo = MagicMock()

    responses = {
        case_path: [Path("case.dvc")],
        reference_path: [Path("reference.dvc")],
        doc_path: [Path("doc.dvc")],
    }

    failure_map = {
        "case": case_path,
        "reference": reference_path,
        "doc": doc_path,
    }
    responses[failure_map[fail_target]] = []

    # Act & Assert
    fake_add_directory = _fake_add_directory_factory(responses)
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        with pytest.raises(RuntimeError, match=expected_message):
            testcase.add_to_dvc(repo)


def test_add_to_dvc_success(fs: FakeFilesystem) -> None:
    """Validate add_to_dvc returns all DVC files when successful."""
    # Arrange
    testcase = __make_testcase()
    case_path, reference_path, doc_path = __make_testcase_paths(fs, testcase)
    repo = MagicMock()

    responses = {
        case_path: [Path("case.dvc")],
        reference_path: [Path("reference.dvc")],
        doc_path: [Path("doc.dvc")],
    }

    # Act
    fake_add_directory = _fake_add_directory_factory(responses)
    with patch("tools.minio_dvc_migration.testcase_data.add_directory_to_dvc", fake_add_directory):
        result = testcase.add_to_dvc(repo)

    # Assert
    assert [path.name for path in result] == ["case.dvc", "reference.dvc", "doc.dvc"]
