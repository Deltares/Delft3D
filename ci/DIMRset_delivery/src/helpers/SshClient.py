import paramiko


class SshClient(object):
    """
    Class to wrap a paramiko ssh client.
    """

    def __init__(self, username: str, password: str):
        """
        Creates a new instance of SshClient.

        Args:
            username (str): Username to create a SSH connection.
            password (str): Password to create a SSH connection.
        """
        self.__username = username
        self.__password = password

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
            self.__client.connect(address, username=self.__username, password=self.__password)
        except Exception as e:
            raise AssertionError(f"Could not establish ssh connection to {address}:\n{e}")
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
            self.__client.connect(address, username=self.__username, password=self.__password)
            self.__client.exec_command(command)
        except Exception as e:
            raise AssertionError(f"Could not execute command '{command}' on '{address}':\n{e}")
        finally:
            self.__client.close()