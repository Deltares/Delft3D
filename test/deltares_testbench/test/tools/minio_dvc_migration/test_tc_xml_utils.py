"""Tests for tc_xml_utils module."""

from pathlib import Path

import pytest

from tools.minio_dvc_migration.tc_xml_utils import load_teamcity_xml_files


def test_load_teamcity_xml_files_empty_file(tmp_path: Path) -> None:
    """Ensure load_teamcity_xml_files rejects empty CSV files."""
    csv_path = tmp_path / "empty.csv"
    csv_path.touch()

    with pytest.raises(ValueError, match="File is empty"):
        load_teamcity_xml_files(str(csv_path))


def test_load_teamcity_xml_files_missing_config_column(tmp_path: Path) -> None:
    """Ensure load_teamcity_xml_files raises when '#config' column is absent."""
    csv_path = tmp_path / "missing_config.csv"
    csv_path.write_text("col1,col2\nvalue1,value2\n", encoding="utf-8")

    with pytest.raises(ValueError, match="Missing '#config' column"):
        load_teamcity_xml_files(str(csv_path))
