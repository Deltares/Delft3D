import sys
from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.lib.git_client import GitClient


def test_tag_commit_success(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run = Mock()
    mock_run.return_value.returncode = 0
    monkeypatch.setattr("subprocess.run", mock_run)
    # Act
    client.tag_commit("abc123", "v1.0.0")
    # Assert
    assert mock_run.call_count == 2
    args1 = mock_run.call_args_list[0][0][0]
    assert args1[:3] == ["git", "tag", "v1.0.0"]
    args2 = mock_run.call_args_list[1][0][0]
    assert args2[:3] == ["git", "push", "--tags"]


def test_tag_commit_fail_tag(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run = Mock()
    mock_run.side_effect = [Mock(returncode=1), Mock(returncode=0)]
    monkeypatch.setattr("subprocess.run", mock_run)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


def test_tag_commit_fail_push(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run = Mock()
    mock_run.side_effect = [Mock(returncode=0), Mock(returncode=1)]
    monkeypatch.setattr("subprocess.run", mock_run)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


def test_tag_commit_exception(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")

    def raise_exc(*a, **kw):
        raise Exception("fail")

    monkeypatch.setattr("subprocess.run", raise_exc)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


def test_test_connection_success(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run = Mock(return_value=Mock(returncode=0))
    monkeypatch.setattr("subprocess.run", mock_run)
    # Act
    client.test_connection(dry_run=False)
    # Assert
    mock_run.assert_called_once()


def test_test_connection_fail(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    mock_run = Mock(return_value=Mock(returncode=1))
    monkeypatch.setattr("subprocess.run", mock_run)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.test_connection(dry_run=False)
        # Assert
        mock_exit.assert_called_once()


def test_test_connection_exception(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")

    def raise_exc(*a, **kw):
        raise Exception("fail")

    monkeypatch.setattr("subprocess.run", raise_exc)
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.test_connection(dry_run=False)
        # Assert
        mock_exit.assert_called_once()


def test_test_connection_dry_run(monkeypatch):
    # Arrange
    client = GitClient("https://repo.url", "user", "pass")
    # Act
    client.test_connection(dry_run=True)
    # Assert
    # No exception should be raised
    assert True
