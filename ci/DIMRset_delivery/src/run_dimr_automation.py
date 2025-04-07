"""
This script automates the weekly DIMR release process. This includes
updating the public wiki, downloading the artifacts to the network drive,
installing the new DIMR set on Linux, pinning and tagging the appropriate builds,
updating SVN, and preparing a mail for the release notification.

This script makes some assumptions:
- The latest successful DIMR build, is the build that we are going to release.
- The second latest successful build, is the build that was released last week.
"""

import argparse
from getpass import getpass

from DimrAutomation import DimrAutomation
from helpers.GitClient import GitClient
from helpers.SshClient import SshClient
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import DELFT3D_GIT_REPO

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="--username and --password are optional for automation"
    )

    parser.add_argument("--username", type=str, default=None)
    parser.add_argument("--password", type=str, default=None)
    parser.add_argument("--git-PAT", type=str, default=None)

    args = parser.parse_args()
    username = args.username
    password = args.password
    personal_access_token = args.git_PAT

    if not username or not password or not personal_access_token:
        username = input("Enter your Deltares username:")
        password = getpass(prompt="Enter your Deltares password:", stream=None)
        personal_access_token = getpass(prompt="Enter your git PAT:", stream=None)

    atlassian_wrapper = Atlassian(username=username, password=password)
    teamcity_wrapper = TeamCity(username=username, password=password)
    ssh_client_wrapper = SshClient(
        username=username, password=password, connect_timeout=30
    )
    git_client_wrapper = GitClient(DELFT3D_GIT_REPO, username, personal_access_token)

    dimr_automation = DimrAutomation(
        atlassian=atlassian_wrapper,
        teamcity=teamcity_wrapper,
        ssh_client=ssh_client_wrapper,
        git_client=git_client_wrapper,
    )
    print("Starting the automation process...")
    dimr_automation.run()
    print("Finished")
