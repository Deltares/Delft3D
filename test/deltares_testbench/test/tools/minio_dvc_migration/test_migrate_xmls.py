"""Unit tests for migrate_xmls utilities."""

import io
from pathlib import Path

from tools.minio_dvc_migration.migrate_xmls import TeeStream, _rotate_log_file


def test_tee_stream_writes_to_all_streams() -> None:
    """TeeStream writes the same data to every provided stream."""
    s1 = io.StringIO()
    s2 = io.StringIO()
    tee = TeeStream(s1, s2)

    tee.write("hello")

    assert s1.getvalue() == "hello"
    assert s2.getvalue() == "hello"


def test_tee_stream_returns_length() -> None:
    """TeeStream.write returns the number of characters written."""
    tee = TeeStream(io.StringIO())

    result = tee.write("hello")

    assert result == 5


def test_rotate_log_file_renames_existing(tmp_path: Path) -> None:
    """_rotate_log_file moves the existing file to .1 and leaves no original."""
    log_file = tmp_path / "test.log"
    log_file.write_text("run 1")

    _rotate_log_file(log_file)

    rotated = tmp_path / "test.log.1"
    assert rotated.exists()
    assert rotated.read_text() == "run 1"
    # Original is now empty (RotatingFileHandler creates a new empty file)
    assert log_file.read_text() == ""


def test_rotate_log_file_chains_rotations(tmp_path: Path) -> None:
    """Successive rotations produce .1, .2, etc."""
    log_file = tmp_path / "test.log"

    log_file.write_text("run 1")
    _rotate_log_file(log_file)

    log_file.write_text("run 2")
    _rotate_log_file(log_file)

    assert (tmp_path / "test.log.2").read_text() == "run 1"
    assert (tmp_path / "test.log.1").read_text() == "run 2"


def test_rotate_log_file_noop_when_missing(tmp_path: Path) -> None:
    """_rotate_log_file does nothing when the file doesn't exist."""
    log_file = tmp_path / "nonexistent.log"

    _rotate_log_file(log_file)  # should not raise

    assert not log_file.exists()
