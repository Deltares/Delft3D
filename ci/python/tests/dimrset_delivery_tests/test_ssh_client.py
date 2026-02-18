from unittest.mock import Mock, patch

import pytest

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.ssh_client import Direction, SshClient
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.example_utils.logger import LogLevel


def test_test_connection_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch.object(client._client, "close") as mock_close,
    ):
        # Act
        client.test_connection()
        # Assert
        mock_connect.assert_called_once_with(hostname="host", username="user", password="pass", timeout=30)
        mock_close.assert_called_once()


def test_test_connection_dry_run() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = True
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    mock_context.settings.dry_run_prefix = "[TEST]"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch.object(client._client, "close") as mock_close,
    ):
        # Act
        client.test_connection()
        # Assert
        mock_connect.assert_not_called()
        mock_close.assert_not_called()


def test_test_connection_fail() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect", side_effect=Exception("fail")),
        patch.object(client._client, "close") as mock_close,
    ):
        # Act & Assert
        result = client.test_connection()
        assert not result
        mock_close.assert_called_once()
        mock_context.log.assert_called_with("Close SSH connection to 'host' with 'user'.")


def test_execute_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch.object(client._client, "exec_command") as mock_exec,
        patch.object(client._client, "close") as mock_close,
    ):
        # Act
        client.execute("ls")
        # Assert
        mock_connect.assert_called_once_with("host", username="user", password="pass", timeout=30)
        mock_exec.assert_called_once_with("ls")
        mock_close.assert_called_once()


def test_execute_fail() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch.object(client._client, "exec_command", side_effect=Exception("fail")),
        patch.object(client._client, "close") as mock_close,
    ):
        # Act
        client.execute("ls")

        # Assert
        mock_context.log.assert_called_with("Could not execute command 'ls' on 'host':\nfail", severity=LogLevel.ERROR)
        mock_connect.assert_called_once()
        mock_close.assert_called_once()


def test_secure_copy_to_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch("ci_tools.dimrset_delivery.lib.ssh_client.SCPClient") as mock_scp,
        patch.object(client._client, "get_transport") as mock_transport,
        patch.object(client._client, "close") as mock_close,
    ):
        mock_transport.return_value = Mock(sock=Mock())
        mock_scp_inst = mock_scp.return_value.__enter__.return_value
        # Act
        client.secure_copy("local", "remote", direction=Direction.TO)
        # Assert
        mock_connect.assert_called_once()
        mock_scp_inst.put.assert_called_once_with("local", remote_path="remote", recursive=True)
        mock_close.assert_called_once()


def test_secure_copy_from_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch("ci_tools.dimrset_delivery.lib.ssh_client.SCPClient") as mock_scp,
        patch.object(client._client, "get_transport") as mock_transport,
        patch.object(client._client, "close") as mock_close,
    ):
        mock_transport.return_value = Mock(sock=Mock())
        mock_scp_inst = mock_scp.return_value.__enter__.return_value
        # Act
        client.secure_copy("local", "remote", direction=Direction.FROM)
        # Assert
        mock_connect.assert_called_once()
        mock_scp_inst.get.assert_called_once_with("remote", "local")
        mock_close.assert_called_once()


def test_secure_copy_fail() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.settings = Mock(spec=Settings)
    mock_context.settings.linux_address = "host"
    client = SshClient(credentials=Credentials("user", "pass"), context=mock_context)
    with (
        patch.object(client._client, "connect") as mock_connect,
        patch("ci_tools.dimrset_delivery.lib.ssh_client.SCPClient", side_effect=Exception("fail")),
        patch.object(client._client, "get_transport") as mock_transport,
        patch.object(client._client, "close") as mock_close,
    ):
        mock_transport.return_value = Mock(sock=Mock())
        # Act & Assert
        with pytest.raises(AssertionError):
            client.secure_copy("local", "remote", direction=Direction.TO)
        mock_connect.assert_called_once()
        mock_close.assert_called_once()
