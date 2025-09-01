import sys
from unittest.mock import Mock, patch
from unittest.mock import Mock as MockType

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings


@patch("subprocess.run")
def test_tag_commit_success(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    mock_run.return_value.returncode = 0
    # Act
    client.tag_commit("abc123", "v1.0.0")
    # Assert
    assert mock_run.call_count == 2
    args1 = mock_run.call_args_list[0][0][0]
    assert args1[:3] == ["git", "tag", "v1.0.0"]
    args2 = mock_run.call_args_list[1][0][0]
    assert args2[:3] == ["git", "push", "--tags"]


@patch("subprocess.run")
def test_tag_commit_fail_tag(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    mock_run.side_effect = [Mock(returncode=1), Mock(returncode=0)]
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_tag_commit_fail_push(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    mock_run.side_effect = [Mock(returncode=0), Mock(returncode=1)]
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_tag_commit_exception(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)

    def raise_exc(*a: object, **kw: object) -> None:
        raise Exception("fail")

    mock_run.side_effect = raise_exc
    with patch.object(sys, "exit") as mock_exit:
        # Act
        client.tag_commit("abc123", "v1.0.0")
        # Assert
        mock_exit.assert_called_once()


@patch("subprocess.run")
def test_test_connection_success(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    mock_run.return_value = Mock(returncode=0)
    # Act
    client.test_connection()
    # Assert
    mock_run.assert_called_once()


@patch("subprocess.run")
def test_test_connection_fail(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    mock_run.return_value = Mock(returncode=1)

    # Act
    result = client.test_connection()

    # Assert
    assert result is False


@patch("subprocess.run")
def test_test_connection_exception(mock_run: MockType) -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)

    def raise_exc(*a: object, **kw: object) -> None:
        raise Exception("fail")

    mock_run.side_effect = raise_exc

    # Act
    result = client.test_connection()

    # Assert
    assert result is False


def test_test_connection_dry_run() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.delft3d_git_repo = "https://repo.url"
    mock_context.settings.dry_run_prefix = "[TEST]"
    client = GitClient(credentials=Credentials("user", "pass"), context=mock_context)
    # Act
    client.test_connection()
    # Assert
    # No exception should be raised
    assert True
