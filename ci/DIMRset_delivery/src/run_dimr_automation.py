"""
This script automates the weekly DIMR release process. This includes
updating the public wiki, downloading the artifacts to the network drive,
installing the new DIMR set on Linux, pinning and tagging the appropriate builds,
updating SVN, and preparing a mail for the release notification.

This script makes some assumptions:
- The latest successful DIMR build, is the build that we are going to release.
- The second latest successful build, is the build that was released last week.
"""

from getpass import getpass

from DimrAutomation import DimrAutomation
from helpers.SshClient import SshClient
from helpers.GitClient import GitClient
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import DELFT3D_GIT_REPO

if __name__ == "__main__":
    username = input("Enter your Deltares username:")
    password = getpass(prompt="Enter your Deltares password:", stream=None)

    atlassian_wrapper = Atlassian(username=username, password=password)
    teamcity_wrapper = TeamCity(username=username, password=password)
    ssh_client_wrapper = SshClient(username=username, password=password)
    git_client_wrapper = GitClient(DELFT3D_GIT_REPO, username, password)

    dimr_automation = DimrAutomation(atlassian=atlassian_wrapper, teamcity=teamcity_wrapper,
                                     ssh_client=ssh_client_wrapper, git_client=git_client_wrapper)
    print("Starting the automation process...")
    dimr_automation.run()
    print("Finished")
