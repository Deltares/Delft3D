import paramiko
from scp import SCPClient
from enum import Enum

class Direction(Enum):
    TO = "to"     # local to remote
    FROM = "from" # remote to local


class SshClient(object):
    """
    Class to wrap a paramiko ssh client.
    """

    def __init__(self, username: str, password: str, connect_timeout: int = 30):
        """
        Creates a new instance of SshClient.

        Args:
            username (str): Username to create a SSH connection.
            password (str): Password to create a SSH connection.
        """
        self.__username = username
        self.__password = password
        self.__connect_timeout = connect_timeout

        self.__client = paramiko.SSHClient()
        self.__client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

    def test_connection(self, address: str) -> None:
        """
        Test if a SSH connection can be made to the specified address.

        Args:
            address:

        Raises:
            AssertionError: If the SSH connection failed.
        """
        try:
            self.__client.connect(
                address,
                username=self.__username,
                password=self.__password,
                timeout=self.__connect_timeout
            )
        except Exception as e:
            raise AssertionError(
                f"Could not establish ssh connection to {address}:\n{e}"
            )
        finally:
            self.__client.close()

    def execute(self, address: str, command: str) -> None:
        """
        Executes the specified command on the specified address.

        Args:
            address (str): The SSH address to connect to.
            command (str): The command to execute on the specified address.

        Raises:
            AssertionError: If the command fails to execute on the specified address.
        """
        try:
            self.__client.connect(
                address,
                username=self.__username,
                password=self.__password,
                timeout=self.__connect_timeout
            )
            self.__client.exec_command(command)
        except Exception as e:
            raise AssertionError(
                f"Could not execute command '{command}' on '{address}':\n{e}"
            )
        finally:
            self.__client.close()

    def secure_copy(
        self,
        address: str,
        local_path: str,
        remote_path: str,
        direction: Direction = Direction.TO
    ) -> None:
        """
        Copies a file to or from the specified address using SCP.

        Args:
            address (str): The SSH address to connect to.
            local_path (str): The local file path.
            remote_path (str): The remote file path.
            direction (Direction): The direction of the copy (Direction.TO or Direction.FROM).

        Raises:
            AssertionError: If the SCP operation fails.
        """
        try:
            self.__client.connect(
                address,
                username=self.__username,
                password=self.__password,
                timeout=self.__connect_timeout
            )
            transport = self.__client.get_transport()
            transport.set_keepalive(60)
            transport.sock.settimeout(120)
            with SCPClient(self.__client.get_transport()) as scp_client:
                scp_client.channel.settimeout(120)
                if direction == Direction.TO:
                    scp_client.put(
                        local_path,
                        remote_path=remote_path,
                        recursive=True
                    )
                elif direction == Direction.FROM:
                    scp_client.get(remote_path, local_path)
                else:
                    raise ValueError(
                        "Invalid direction. Use 'to' for local to remote or 'from' for remote to local."
                    )
        except Exception as e:
            raise AssertionError(
                f"Could not perform SCP operation '{direction}' on '{address}':\n{e}"
            )
        finally:
            self.__client.close()
