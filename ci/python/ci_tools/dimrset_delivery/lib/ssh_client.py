from enum import Enum

import paramiko
from scp import SCPClient

from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings  # type: ignore[import-untyped]


class Direction(Enum):
    """Enumeration for SCP copy direction."""

    TO = "to"  # local to remote
    FROM = "from"  # remote to local


class SshClient:
    """Class to wrap a paramiko ssh client."""

    def __init__(self, username: str, password: str, settings: Settings, connect_timeout: int = 30) -> None:
        """
        Create a new instance of SshClient.

        Parameters
        ----------
        username : str
            Username to create a SSH connection.
        password : str
            Password to create a SSH connection.
        """
        self.__username = username
        self.__password = password
        self.__connect_timeout = connect_timeout

        self._client = paramiko.SSHClient()
        self._client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        self.__settings = settings
        self.__address = settings.linux_address

    def test_connection(self, dry_run: bool) -> None:
        """
        Test if a SSH connection can be made to the specified address.

        Parameters
        ----------
        dry_run : bool
            If True, the connection will not be established, but a message will be printed.

        Raises
        ------
        AssertionError
            If the SSH connection failed.
        """
        try:
            if dry_run:
                print(f"{self.__settings.dry_run_prefix} SSH connection to {self.__address} with {self.__username}")
            else:
                self._client.connect(
                    self.__address, username=self.__username, password=self.__password, timeout=self.__connect_timeout
                )
            print(f"Successfully created and closed a ssh connection to {self.__address} with {self.__username}.")
        except Exception as e:
            raise AssertionError(f"Could not establish ssh connection to {self.__address}:\n{e}") from e
        finally:
            self._client.close()

    def execute(self, command: str) -> None:
        """
        Execute the specified command on the specified address.

        Parameters
        ----------
        command : str
            The command to execute on the specified address.

        Raises
        ------
        AssertionError
            If the command fails to execute on the specified address.
        """
        try:
            self._client.connect(
                self.__address, username=self.__username, password=self.__password, timeout=self.__connect_timeout
            )
            self._client.exec_command(command)
        except Exception as e:
            raise AssertionError(f"Could not execute command '{command}' on '{self.__address}':\n{e}") from e
        finally:
            self._client.close()
            print(f"Successfully executed command '{command}' on '{self.__address}'.")

    def secure_copy(self, local_path: str, remote_path: str, direction: Direction = Direction.TO) -> None:
        """
        Copy a file to or from the specified address using SCP.

        Parameters
        ----------
        local_path : str
            The local file path.
        remote_path : str
            The remote file path.
        direction : Direction
            The direction of the copy (Direction.TO or Direction.FROM).

        Raises
        ------
        AssertionError
            If the SCP operation fails.
        """
        try:
            self._client.connect(
                self.__address, username=self.__username, password=self.__password, timeout=self.__connect_timeout
            )
            transport = self._client.get_transport()
            if transport is not None:
                transport.set_keepalive(60)
                if transport.sock is not None:
                    transport.sock.settimeout(120)
            transport = self._client.get_transport()
            if transport is None:
                raise AssertionError(f"Could not get SSH transport for SCP operation '{direction}' on '{address}'")
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
