from enum import Enum

import paramiko
from scp import SCPClient  # type: ignore

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface
from ci_tools.example_utils.logger import LogLevel


class Direction(Enum):
    """Enumeration for SCP copy direction.

    Used to specify the direction of file transfer in SCP operations.
    """

    TO = "to"  # local to remote
    FROM = "from"  # remote to local


class SshClient(ConnectionServiceInterface):
    """Wraps a Paramiko SSH client for remote command execution and file transfer.

    Provides methods to test SSH connectivity, execute remote commands, and transfer files using SCP.
    """

    def __init__(self, credentials: Credentials, context: DimrAutomationContext, connect_timeout: int = 30) -> None:
        """Initialize a new SshClient instance.

        Parameters
        ----------
        credentials : Credentials
            Username and Password for authentication.
        context : DimrAutomationContext
            Context object containing settings and logging utilities.
        connect_timeout : int, optional
            Timeout for SSH connection in seconds (default is 30).
        """
        self.__credentials = credentials
        self.__connect_timeout = connect_timeout

        self._client = paramiko.SSHClient()
        self._client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        self.__context = context
        self.__address = context.settings.linux_address

    def test_connection(self) -> bool:
        """Test the SSH connection to the specified address.

        Returns
        -------
        bool
            True if the connection test is successful or dry run is performed, False otherwise.
        """
        if self.__context.dry_run:
            self.__context.log(f"SSH connection to '{self.__address}' with '{self.__credentials.username}'")
            success = True
        else:
            try:
                self._client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
                self._client.connect(
                    hostname=self.__address,
                    username=self.__credentials.username,
                    password=self.__credentials.password,
                    timeout=self.__connect_timeout,
                )
                self.__context.log(
                    f"Successfully created an SSH connection to '{self.__address}' "
                    f"with '{self.__credentials.username}'."
                )
                success = True
            except Exception as e:
                self.__context.log(
                    f"Failed to establish SSH connection to '{self.__address}': {e}", severity=LogLevel.ERROR
                )
                success = False
            finally:
                self._client.close()
                self.__context.log(f"Close SSH connection to '{self.__address}' with '{self.__credentials.username}'.")
        return success

    def execute(self, command: str) -> bool:
        """Execute a command on the remote address."""
        success = False
        try:
            self._client.connect(
                self.__address,
                username=self.__credentials.username,
                password=self.__credentials.password,
                timeout=self.__connect_timeout,
            )
            _, stdout, _ = self._client.exec_command(command)
            exit_status = stdout.channel.recv_exit_status()
            if exit_status == 0:
                self.__context.log(f"Successfully executed command '{command}' on '{self.__address}'.")
                success = True
            else:
                self.__context.log(
                    f"Command '{command}' failed with exit status {exit_status} on '{self.__address}'.",
                    severity=LogLevel.ERROR,
                )
        except Exception as e:
            self.__context.log(
                f"Could not execute command '{command}' on '{self.__address}':\n{e}", severity=LogLevel.ERROR
            )
        finally:
            self._client.close()
        return success

    def secure_copy(self, local_path: str, remote_path: str, direction: Direction = Direction.TO) -> None:
        """Copy a file to or from the remote address using SCP.

        Parameters
        ----------
        local_path : str
            Local file path.
        remote_path : str
            Remote file path.
        direction : Direction, optional
            Direction of the copy (Direction.TO for local to remote, Direction.FROM for remote to local).

        Raises
        ------
        AssertionError
            If the SCP operation fails.
        ValueError
            If the direction argument is invalid.
        """
        try:
            self._client.connect(
                self.__address,
                username=self.__credentials.username,
                password=self.__credentials.password,
                timeout=self.__connect_timeout,
            )
            transport = self._client.get_transport()
            if transport is not None:
                transport.set_keepalive(60)
                if transport.sock is not None:
                    transport.sock.settimeout(120)
            transport = self._client.get_transport()
            if transport is None:
                raise AssertionError(
                    f"Could not get SSH transport for SCP operation '{direction}' on '{self.__address}'"
                )
            with SCPClient(transport) as scp_client:
                if hasattr(scp_client, "channel") and scp_client.channel is not None:
                    scp_client.channel.settimeout(120)
                if direction == Direction.TO:
                    scp_client.put(local_path, remote_path=remote_path, recursive=True)
                elif direction == Direction.FROM:
                    scp_client.get(remote_path, local_path)
                else:
                    raise ValueError("Invalid direction. Use 'to' for local to remote or 'from' for remote to local.")
        except Exception as e:
            raise AssertionError(f"Could not perform SCP operation '{direction}' on '{self.__address}':\n{e}") from e
        finally:
            self._client.close()
