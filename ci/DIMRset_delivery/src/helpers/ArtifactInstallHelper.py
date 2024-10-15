import os
from shutil import unpack_archive
from typing import List

from helpers.SshClient import SshClient
from lib.TeamCity import TeamCity
from settings.general_settings import NETWORK_BASE_PATH, LINUX_ADDRESS
from settings.teamcity_settings import DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID
from settings.teamcity_settings import NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT
from settings.teamcity_settings import NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT


class ArtifactInstallHelper(object):
    """ Class responsible for downloading, unpacking and installing the DIMR artifacts. """

    def __init__(self, teamcity: TeamCity, ssh_client: SshClient, dimr_version: str, branch_name: str):
        """
        Creates a new instance of ArtifactInstallHelper.

        Arguments:
            teamcity (TeamCity): A reference to a TeamCity REST API wrapper.
            ssh_client (Ssh_Client): A wrapper for a SSH client.
            full_dimr_version (str): The full DIMR version to download and install.
        """
        self.__teamcity = teamcity
        self.__ssh_client = ssh_client
        self.__dimr_version = dimr_version
        self.__branch_name = branch_name

    def download_artifacts_to_network_drive(self) -> None:
        """ Downloads the DIMR artifacts to the network drive. """
        latest_dimr_collector_release_signed_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID)
        artifact_names = \
            self.__teamcity.get_build_artifact_names(build_id=latest_dimr_collector_release_signed_build_id)

        artifacts_to_download = []

        for artifact_name in artifact_names["file"]:
            if NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT in artifact_name["name"] \
                    or NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT in artifact_name["name"]:
                artifacts_to_download.append(artifact_name["name"])

        self.__download_and_unpack_dimr_artifacts(artifacts_to_download=artifacts_to_download,
                                                  build_id=latest_dimr_collector_release_signed_build_id)

    def install_dimr_on_linux(self) -> None:
        """ Installs DIMR on the Linux machine via SSH. """
        print(f"Installing DIMR on {LINUX_ADDRESS} via SSH...")

        # setup the command
        command = f"cd /p/d-hydro/dimrset/weekly/{self.__dimr_version}/lnx64/bin;"
        command += "./libtool_install.sh;"
        command += "cd /p/d-hydro/dimrset/weekly;"
        command += f"chgrp -R dl_acl_dsc {self.__dimr_version}/;"
        command += f"chmod -R a+x,a-s {self.__dimr_version}/;"
        if self.__branch_name == 'main':
            command += "unlink latest;"
            command += f"ln -s {self.__dimr_version} latest;" 
            command += "cd /p/d-hydro/dimrset;"
            command += "unlink latest;"
            command += f"ln -s weekly/{self.__dimr_version} latest;"

        # execute command
        self.__ssh_client.execute(address=LINUX_ADDRESS, command=command)
        print(f"Successfully installed DIMR on {LINUX_ADDRESS}.")


    def __download_and_unpack_dimr_artifacts(self, artifacts_to_download: List[str], build_id: str) -> None:
        """
        Downloads the provided artifact names from the provided TeamCity build id.

        Args:
            artifacts_to_download List[str]: A list of artifact names to download.
            build_id (str): The build id for the build to download the artifacts from.
        """
        file_path = f"{NETWORK_BASE_PATH}{self.__dimr_version}"
        if not os.path.exists(f"{file_path}"):
            command = f"mkdir /p/d-hydro/dimrset/weekly/{self.__dimr_version}"
            self.__ssh_client.execute(address=LINUX_ADDRESS, command=command)  # via SSH due to permission issues on P:\

        for artifact_to_download in artifacts_to_download:
            print(f"Downloading {artifact_to_download}...")
            artifact = self.__teamcity.get_build_artifact(build_id=build_id,
                                                          path_to_artifact=artifact_to_download)
            artifact_path = f"{file_path}\\{artifact_to_download}"
            with open(artifact_path, 'wb') as f:
                f.write(artifact)
            print(f"Unpacking {artifact_to_download}...")
            unpack_archive(artifact_path, extract_dir=file_path)
            print(f"Deleting {artifact_to_download}...")
            os.remove(artifact_path)
            print(f"Successfully downloaded {artifact_to_download}.")