import os
from contextlib import contextmanager
from typing import Iterator
from unittest.mock import MagicMock

import pytest
from pytest_mock import MockerFixture

from src.config.credentials import Credentials
from src.utils.handlers.dvc_handler import _AWS_KEYS, DvcHandler
from src.utils.logging.i_logger import ILogger


@contextmanager
def clean_aws_env() -> Iterator[None]:
    """Ensure AWS env vars are cleaned up even if a test fails."""
    saved = {k: os.environ.get(k) for k in _AWS_KEYS}
    try:
        yield
    finally:
        for k, v in saved.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v


def _make_credentials(username: str = "ak", password: str = "sk") -> Credentials:
    creds = Credentials()
    creds.username = username
    creds.password = password
    return creds


class TestDvcHandlerDownloadBatch:
    def test_empty_list_does_nothing(self) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        handler.download_batch([], Credentials(), logger)

        repo.fetch.assert_not_called()
        repo.checkout.assert_not_called()

    def test_sets_aws_credentials_from_credentials_object(self, mocker: MockerFixture, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)
        creds = _make_credentials("my_key", "my_secret")

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")

        # Make checkout create the expected output dir
        expected_dir = os.path.splitext(dvc_file)[0]
        os.makedirs(expected_dir)

        captured_env: dict[str, str | None] = {}

        def capture_env_on_fetch(**kwargs: object) -> int:
            captured_env["key"] = os.environ.get(_AWS_KEYS[0])
            captured_env["secret"] = os.environ.get(_AWS_KEYS[1])
            return 0

        repo.fetch.side_effect = capture_env_on_fetch

        with clean_aws_env():
            os.environ.pop(_AWS_KEYS[0], None)
            os.environ.pop(_AWS_KEYS[1], None)

            handler.download_batch([dvc_file], creds, logger)

            assert captured_env["key"] == "my_key"
            assert captured_env["secret"] == "my_secret"

    def test_restores_env_after_success(self, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)
        creds = _make_credentials()

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")
        os.makedirs(os.path.splitext(dvc_file)[0])

        with clean_aws_env():
            os.environ[_AWS_KEYS[0]] = "original_key"
            os.environ[_AWS_KEYS[1]] = "original_secret"

            handler.download_batch([dvc_file], creds, logger)

            assert os.environ[_AWS_KEYS[0]] == "original_key"
            assert os.environ[_AWS_KEYS[1]] == "original_secret"

    def test_restores_env_after_failure(self, tmp_path: object) -> None:
        repo = MagicMock()
        repo.fetch.side_effect = RuntimeError("network error")
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)
        creds = _make_credentials()

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")

        with clean_aws_env():
            os.environ[_AWS_KEYS[0]] = "original_key"
            os.environ[_AWS_KEYS[1]] = "original_secret"

            with pytest.raises(RuntimeError, match="network error"):
                handler.download_batch([dvc_file], creds, logger)

            assert os.environ[_AWS_KEYS[0]] == "original_key"
            assert os.environ[_AWS_KEYS[1]] == "original_secret"

    def test_raises_file_not_found_for_missing_dvc_file(self) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        with pytest.raises(FileNotFoundError, match="DVC file not found"):
            handler.download_batch(["/nonexistent/input.dvc"], Credentials(), logger)

    def test_calls_fetch_and_checkout_with_all_targets(self, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        dvc_file_1 = os.path.join(str(tmp_path), "input.dvc")
        dvc_file_2 = os.path.join(str(tmp_path), "reference_win64.dvc")
        for f in [dvc_file_1, dvc_file_2]:
            with open(f, "w") as fh:
                fh.write("md5: abc\n")
            os.makedirs(os.path.splitext(f)[0])

        handler.download_batch([dvc_file_1, dvc_file_2], Credentials(), logger)

        # Both targets passed to single fetch and checkout call
        repo.fetch.assert_called_once()
        fetch_targets = repo.fetch.call_args[1]["targets"]
        assert len(fetch_targets) == 2

        repo.checkout.assert_called_once()
        checkout_targets = repo.checkout.call_args[1]["targets"]
        assert len(checkout_targets) == 2

    def test_passes_jobs_parameter_to_fetch(self, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")
        os.makedirs(os.path.splitext(dvc_file)[0])

        handler.download_batch([dvc_file], Credentials(), logger, jobs=8)

        assert repo.fetch.call_args[1]["jobs"] == 8

    def test_raises_when_output_dir_missing_after_checkout(self, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")
        # Do NOT create the expected output dir — simulates failed checkout

        with pytest.raises(FileNotFoundError, match="expected directories"):
            handler.download_batch([dvc_file], Credentials(), logger)


class TestDvcHandlerDownload:
    def test_download_delegates_to_pull_with_credentials(self, mocker: MockerFixture, tmp_path: object) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)
        creds = _make_credentials("user", "pass")

        dvc_file = os.path.join(str(tmp_path), "input.dvc")
        with open(dvc_file, "w") as f:
            f.write("md5: abc\n")
        os.makedirs(os.path.join(str(tmp_path), "input"))

        handler.download(dvc_file, "/unused", creds, None, logger)

        repo.fetch.assert_called_once()
        repo.checkout.assert_called_once()

    def test_download_raises_for_missing_file(self) -> None:
        repo = MagicMock()
        handler = DvcHandler(repo=repo)
        logger = MagicMock(spec=ILogger)

        with pytest.raises(FileNotFoundError, match="DVC file not found"):
            handler.download("/nonexistent.dvc", "/unused", Credentials(), None, logger)


class TestDvcHandlerFindRoot:
    def test_finds_dvc_root_from_nested_path(self, tmp_path: object) -> None:
        dvc_dir = os.path.join(str(tmp_path), ".dvc")
        os.makedirs(dvc_dir)
        nested = os.path.join(str(tmp_path), "a", "b", "c")
        os.makedirs(nested)

        # Create a dummy file so os.path.dirname works as start path
        start = os.path.join(nested, "dummy.txt")
        with open(start, "w") as f:
            f.write("")

        handler = DvcHandler.__new__(DvcHandler)
        root = handler._DvcHandler__find_dvc_root(start)  # type: ignore[attr-defined]
        assert os.path.realpath(str(tmp_path)) == root

    def test_raises_when_no_dvc_dir(self, tmp_path: object) -> None:
        start = os.path.join(str(tmp_path), "dummy.txt")
        with open(start, "w") as f:
            f.write("")

        handler = DvcHandler.__new__(DvcHandler)
        with pytest.raises(ValueError, match="Could not find DVC repository root"):
            handler._DvcHandler__find_dvc_root(start)  # type: ignore[attr-defined]

    def test_finds_dvc_root_when_started_from_directory(self, tmp_path: object) -> None:
        dvc_dir = os.path.join(str(tmp_path), ".dvc")
        os.makedirs(dvc_dir)

        handler = DvcHandler.__new__(DvcHandler)
        root = handler._DvcHandler__find_dvc_root(str(tmp_path))  # type: ignore[attr-defined]
        assert os.path.realpath(str(tmp_path)) == root
