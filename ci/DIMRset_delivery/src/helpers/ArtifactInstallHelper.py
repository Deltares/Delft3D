import os
import tarfile
import zipfile
from typing import List

from helpers.SshClient import SshClient
from lib.TeamCity import TeamCity
from settings.general_settings import LINUX_ADDRESS, NETWORK_BASE_PATH
from settings.teamcity_settings import (
    NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT,
    NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT,
    TEAMCITY_IDS,
)


def extract_archive(archive_path: str, target_path: str):
    """
    Extracts a tar or zip archive to a target path.

    Parameters
    ----------
    archive_path: str
        The path to the archive (tar or zip).
    target_path: str
        The path to extract the archive to.

    Notes
    -----
    - The function removes the Unix permission bits when on Windows, as the permission bits are not supported on Windows.
    """
    if archive_path.endswith(".tar.gz") or archive_path.endswith(".tgz"):
        with tarfile.open(archive_path, "r:gz") as tar:
            for member in tar.getmembers():
                # Strip the Unix permission bits when on Windows
                if os.name == "nt":
                    member.mode = None
                tar.extract(member, path=target_path)
    elif archive_path.endswith(".zip"):
        with zipfile.ZipFile(archive_path, "r") as zip_ref:
            zip_ref.extractall(target_path)
    else:
        raise ValueError(
            "Unsupported archive format. Only .tar.gz, .tgz, and .zip are supported."
        )


class ArtifactInstallHelper(object):
    """Class responsible for downloading, unpacking and installing the DIMR artifacts."""

    def __init__(
        self,
        teamcity: TeamCity,
        ssh_client: SshClient,
        dimr_version: str,
        branch_name: str,
    ):
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

    def publish_artifacts_to_network_drive(self, build_id_chain: str) -> None:
        """Downloads the DIMR artifacts to the network drive."""
        windows_collect_id = self.__teamcity.get_dependent_build_id(
            build_id_chain, TEAMCITY_IDS.DELFT3D_WINDOWS_COLLECT_BUILD_TYPE_ID.value
        )
        linux_collect_id = self.__teamcity.get_dependent_build_id(
            build_id_chain, TEAMCITY_IDS.DELFT3D_LINUX_COLLECT_BUILD_TYPE_ID.value
        )

        self.__publish_artifact_to_file_share(
            windows_collect_id, NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT
        )
        self.__publish_artifact_to_file_share(
            linux_collect_id, NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT
        )

    def publish_weekly_dimr_via_h7(self) -> None:
        """Installs DIMR on the Linux machine via SSH."""
        print(f"Installing DIMR on {LINUX_ADDRESS} via SSH...")

        # setup the command
        command = f"cd /p/d-hydro/dimrset/weekly/{self.__dimr_version}/lnx64/bin;"
        command += "./libtool_install.sh;"
        command += "cd /p/d-hydro/dimrset/weekly;"
        command += f"chgrp -R dl_acl_dsc {self.__dimr_version}/;"
        command += f"chmod -R a+x,a-s {self.__dimr_version}/;"

        # only update the latest symlink if we are on the main branch we don't do this for the release branche
        if self.__branch_name == "main":
            command += "unlink latest;"
            command += f"ln -s {self.__dimr_version} latest;"
            command += "cd /p/d-hydro/dimrset;"
            command += "unlink latest;"
            command += f"ln -s weekly/{self.__dimr_version} latest;"

        # execute command
        self.__ssh_client.execute(address=LINUX_ADDRESS, command=command)
        print(f"Successfully installed DIMR on {LINUX_ADDRESS}.")

    def __publish_artifact_to_file_share(self, build_id, artifact_name_key):
        """
        Downloads and unpacks artifacts from a TeamCity build that match the specified artifact name key.

        Args:
            build_id (str): The ID of the TeamCity build to retrieve artifacts from.
            artifact_name_key (str): Substring to filter artifact names for downloading.

        Returns:
            None
        """
        artifact_names = self.__teamcity.get_build_artifact_names(build_id=build_id)
        artifacts_to_download = [
            a["name"] for a in artifact_names["file"] if artifact_name_key in a["name"]
        ]
        self.__download_and_unpack_dimr_artifacts_via_h7(
            artifacts_to_download=artifacts_to_download,
            build_id=build_id,
        )

    def __download_and_unpack_dimr_artifacts_via_h7(
        self, artifacts_to_download: List[str], build_id: str
    ) -> None:
        """
        Downloads the provided artifact names from the provided TeamCity build id.

        Args:
            artifacts_to_download List[str]: A list of artifact names to download.
            build_id (str): The build id for the build to download the artifacts from.
        """
        file_path = f"{self.__dimr_version}"
        if not os.path.exists(f"{file_path}"):
            os.makedirs(f"{file_path}")

        for artifact_to_download in artifacts_to_download:
            print(f"Downloading {artifact_to_download}...")
            artifact = self.__teamcity.get_build_artifact(
                build_id=build_id, path_to_artifact=artifact_to_download
            )

            with open(artifact_to_download, "wb") as f:
                f.write(artifact)
            print(f"Unpacking {artifact_to_download}...")

            extract_archive(artifact_to_download, file_path)
            print(f"Deleting {artifact_to_download}...")
            os.remove(artifact_to_download)

            remote_path = "/p/d-hydro/dimrset/weekly"
            self.__ssh_client.secure_copy(
                address=LINUX_ADDRESS,
                local_path=file_path,
                remote_path=remote_path,
            )
            print(f"Successfully deployed {artifact_to_download} to {remote_path}.")
