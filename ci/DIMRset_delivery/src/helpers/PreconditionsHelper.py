import os

from helpers.SshClient import SshClient
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import NETWORK_BASE_PATH, LINUX_ADDRESS


class PreconditionsHelper(object):
    """ Class to check preconditions before running the main DIMR automation script. """

    def __init__(self, atlassian: Atlassian, teamcity: TeamCity,  ssh_client: SshClient):
        """
        Creates a new instance of PreconditionsHelper.

        Args:
            atlassian (Atlassian): A wrapper for the Atlassian Confluence REST API.
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            ssh_client (SshClient): A wrapper for a SSH client.
        """
        self.__teamcity = teamcity
        self.__atlassian = atlassian
        self.__ssh_client = ssh_client

    def assert_preconditions(self) -> None:
        """ Asserts if all preconditions are met. """
        print("Asserting if all preconditions are met...")
        self.__check_teamcity_api_connection()
        self.__check_atlassian_api_connection()
        self.__check_network_base_path_accessible()
        self.__check_ssh_connection_to_linux()
        print("Successfully asserted all preconditions.")

    def __check_teamcity_api_connection(self) -> None:
        print("Checking connection to the TeamCity API...")
        if not self.__teamcity.test_api_connection():
            raise AssertionError("Failed to connect to the TeamCity REST API.")
        print("Successfully connected to the TeamCity API.")

    def __check_atlassian_api_connection(self) -> None:
        print("Checking connection to the Atlassian Confluence API...")
        if not self.__atlassian.test_api_connection():
            raise AssertionError("Failed to connect to the Atlassian Confluence REST API.")
        print("Successfully connected to the Atlassian Confluence API.")

    def __check_network_base_path_accessible(self) -> None:
        print("Checking read/write access to the network drive...")
        try:
            if os.access(NETWORK_BASE_PATH, os.W_OK) and os.access(NETWORK_BASE_PATH, os.R_OK):
                print(f"Successfully checked for read and write access to {NETWORK_BASE_PATH}.")
                return
        except Exception as e:
            raise AssertionError(f"Could not read or write to {NETWORK_BASE_PATH}:\n{e}.")

    def __check_ssh_connection_to_linux(self) -> None:
        print("Checking if ssh connection to Linux can be made...")
        try:
            self.__ssh_client.test_connection(address=LINUX_ADDRESS)
            print(f"Successfully created and closed a ssh connection to {LINUX_ADDRESS}.")
        except Exception as e:
            raise AssertionError(f"Could not establish ssh connection to {LINUX_ADDRESS}:\n{e}")
