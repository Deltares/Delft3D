"""Tests for dvc_utils module."""

from pathlib import Path
from unittest.mock import MagicMock

import pytest

from tools.minio_dvc_migration.dvc_utils import add_directory_to_dvc


def test_add_directory_to_dvc_missing_path_raises(tmp_path: Path) -> None:
    """Ensure FileNotFoundError when the target path is absent."""
    missing_path = tmp_path / "nonexistent"
    repo = MagicMock()

    with pytest.raises(FileNotFoundError, match="Path does not exist"):
        add_directory_to_dvc(missing_path, repo)

    repo.add.assert_not_called()
