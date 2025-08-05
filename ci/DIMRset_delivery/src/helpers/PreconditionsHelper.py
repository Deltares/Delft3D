import os

from helpers.SshClient import SshClient
from helpers.GitClient import GitClient
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import DRY_RUN_PREFIX, NETWORK_BASE_PATH, LINUX_ADDRESS


class PreconditionsHelper(object):
    """ Class to check preconditions before running the main DIMR automation script. """

    def __init__(self, atlassian: Atlassian, teamcity: TeamCity,  ssh_client: SshClient,  git_client: GitClient):
        """
        Creates a new instance of PreconditionsHelper.

        Args:
            atlassian (Atlassian): A wrapper for the Atlassian Confluence REST API.
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            ssh_client: A wrapper for a SSH client.
            git_client: A wrapper for a Git client.
        """
        self.__teamcity = teamcity
        self.__atlassian = atlassian
        self.__ssh_client = ssh_client
        self.__git_client = git_client

    def assert_preconditions(self, dry_run: bool) -> None:
        """ Asserts if all preconditions are met. """
        print("Asserting if all preconditions are met...")

        if not self.__teamcity.test_api_connection(dry_run):
            raise AssertionError("Failed to connect to the TeamCity REST API.")
        
        if not self.__atlassian.test_api_connection(dry_run):
            raise AssertionError("Failed to connect to the Atlassian Confluence REST API.")

        print("Checking read/write access to the network drive...")
        try:
            if (os.access(NETWORK_BASE_PATH, os.W_OK) and os.access(NETWORK_BASE_PATH, os.R_OK)) or dry_run:
                if dry_run:
                    print(f"{DRY_RUN_PREFIX} Checking read/write access to {NETWORK_BASE_PATH}")
                print(f"Successfully checked for read and write access to {NETWORK_BASE_PATH}.")
            else:
                raise AssertionError(f"Could not read or write to {NETWORK_BASE_PATH}: insufficient permissions.")
        except Exception as e:
            raise AssertionError(f"Could not read or write to {NETWORK_BASE_PATH}:\n{e}.")
        
        print("Checking if ssh connection to Linux can be made...")
        try:
            self.__ssh_client.test_connection(LINUX_ADDRESS, dry_run)
        except Exception as e:
            raise AssertionError(f"Could not establish ssh connection to {LINUX_ADDRESS}:\n{e}")
        
        print("Checking if git connection can be made...")
        try:
            self.__git_client.test_connection(dry_run)
        except Exception as e:
            raise AssertionError(f"Could not establish git connection:\n{e}")
        
        print("Successfully asserted all preconditions.")