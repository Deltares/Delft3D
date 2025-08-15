#!/usr/bin/env python3
"""Download the artifacts and install them on Linux machine."""

import os
import tarfile
import zipfile
from typing import List

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity


def download_and_install_artifacts(context: DimrAutomationContext) -> None:
    """Download the artifacts and install them on Linux machine.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.log("Downloading and installing artifacts...")

    # Get required information
    branch_name = context.get_branch_name()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(
            f"{context.settings.dry_run_prefix} Would download artifacts for build from TeamCity:",
            context.build_id,
        )
        print(f"{context.settings.dry_run_prefix} Would publish artifacts to network drive")
        print(f"{context.settings.dry_run_prefix} Would publish weekly DIMR via H7")
        return

    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")
    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")

    helper = ArtifactInstallHelper(
        teamcity=context.teamcity,
        ssh_client=context.ssh_client,
        dimr_version=dimr_version,
        branch_name=branch_name,
    )
    helper.download_and_deploy_artifacts(context)
    helper.install_dimr_on_remote_system(context.settings.linux_address)

    print("Artifacts download and installation completed successfully!")


class ArtifactInstallHelper:
    """Class responsible for downloading, unpacking and installing the DIMR artifacts."""

    def __init__(
        self,
        teamcity: TeamCity,
        ssh_client: SshClient,
        dimr_version: str,
        branch_name: str,
        remote_base_path: str = "/p/d-hydro/dimrset",
        installation_path: str = "weekly",
    ) -> None:
        """
        Create a new instance of ArtifactInstallHelper.

        Arguments:
            teamcity (TeamCity): A reference to a TeamCity REST API wrapper.
            ssh_client (SshClient): A wrapper for a SSH client.
            dimr_version (str): The full DIMR version to download and install.
            branch_name (str): The branch name for version control decisions.
            remote_base_path (str): Base path for DIMR installation on remote system.
            installation_path (str): Installation subdirectory (e.g., 'weekly', 'release').
        """
        self.__teamcity = teamcity
        self.__ssh_client = ssh_client
        self.__dimr_version = dimr_version
        self.__branch_name = branch_name
        self.__remote_base_path = remote_base_path
        self.__installation_path = installation_path

    def download_and_deploy_artifacts(self, context: DimrAutomationContext) -> None:
        """Download the DIMR artifacts from TeamCity and deploy them to the remote system."""
        windows_collect_id = self.__teamcity.get_dependent_build_id(
            context.build_id, context.settings.teamcity_ids.delft3d_windows_collect_build_type_id
        )

        linux_collect_id = self.__teamcity.get_dependent_build_id(
            context.build_id, context.settings.teamcity_ids.delft3d_linux_collect_build_type_id
        )

        self.__download_and_deploy_artifact_by_name(
            str(windows_collect_id) if windows_collect_id is not None else "",
            context.settings.name_of_dimr_release_signed_windows_artifact,
        )

        self.__download_and_deploy_artifact_by_name(
            str(linux_collect_id) if linux_collect_id is not None else "",
            context.settings.name_of_dimr_release_signed_linux_artifact,
        )

    def install_dimr_on_remote_system(self, linux_address: str) -> None:
        """Install DIMR on the Linux machine via SSH."""
        print(f"Installing DIMR on {linux_address} via SSH...")

        command = self.__build_ssh_installation_command()
        self.__ssh_client.execute(command=command)

    def __build_ssh_installation_command(self) -> str:
        """Build the SSH command for DIMR installation."""
        installation_full_path = f"{self.__remote_base_path}/{self.__installation_path}"
        version_path = f"{installation_full_path}/{self.__dimr_version}"

        command = f"cd {version_path}/lnx64/bin;"
        command += "./libtool_install.sh;"
        command += f"cd {installation_full_path};"
        command += f"chgrp -R dl_acl_dsc {self.__dimr_version}/;"
        command += f"chmod -R a+x,a-s {self.__dimr_version}/;"

        # Only update the latest symlink if we are on the main branch
        if self.__branch_name == "main":
            command += "unlink latest;"
            command += f"ln -s {self.__dimr_version} latest;"
            command += f"cd {self.__remote_base_path};"
            command += "unlink latest;"
            command += f"ln -s {self.__installation_path}/{self.__dimr_version} latest;"

        return command

    def __download_and_deploy_artifact_by_name(self, build_id: str, artifact_name_key: str) -> None:
        """
        Download and unpack artifacts from a TeamCity build that match the specified artifact name key.

        Args:
            build_id (str): The ID of the TeamCity build to retrieve artifacts from.
            artifact_name_key (str): Substring to filter artifact names for downloading.

        Returns
        -------
            None
        """
        artifact_names = self.__teamcity.get_build_artifact_names(build_id=build_id)
        if artifact_names is None:
            raise ValueError(f"Could not retrieve artifact names for build {build_id}")
        artifacts_to_download = [a["name"] for a in artifact_names["file"] if artifact_name_key in a["name"]]
        self.__download_and_unpack_dimr_artifacts(artifacts_to_download=artifacts_to_download, build_id=build_id)

    def __download_and_unpack_dimr_artifacts(self, artifacts_to_download: List[str], build_id: str) -> None:
        """
        Download the provided artifact names from the provided TeamCity build id.

        Args:
            artifacts_to_download (List[str]): A list of artifact names to download.
            build_id (str): The build id for the build to download the artifacts from.
        """
        self.__ensure_version_directory_exists()

        for artifact_name in artifacts_to_download:
            self.__download_extract_and_deploy_artifact(artifact_name, build_id)

    def __ensure_version_directory_exists(self) -> None:
        """Ensure the version directory exists for artifact storage."""
        file_path = f"{self.__dimr_version}"
        if not os.path.exists(file_path):
            os.makedirs(file_path)

    def __download_extract_and_deploy_artifact(self, artifact_name: str, build_id: str) -> None:
        """Download, extract, and deploy a single artifact."""
        print(f"Downloading {artifact_name}...")
        artifact = self.__teamcity.get_build_artifact(build_id=build_id, path_to_artifact=artifact_name)

        if artifact is None:
            raise ValueError(f"Could not download artifact {artifact_name}")

        # Write artifact to local file
        with open(artifact_name, "wb") as f:
            f.write(artifact)

        # Extract and deploy
        print(f"Unpacking {artifact_name}...")
        file_path = f"{self.__dimr_version}"
        self.__extract_archive(artifact_name, file_path)

        print(f"Deleting {artifact_name}...")
        os.remove(artifact_name)

        # Deploy to remote location
        remote_path = f"{self.__remote_base_path}/{self.__installation_path}"
        self.__ssh_client.secure_copy(
            local_path=file_path,
            remote_path=remote_path,
        )
        print(f"Successfully deployed {artifact_name} to {remote_path}.")

    def __extract_archive(self, archive_path: str, target_path: str) -> None:
        """
        Extract a tar or zip archive to a target path.

        Parameters
        ----------
        archive_path: str
            The path to the archive (tar or zip).
        target_path: str
            The path to extract the archive to.

        Notes
        -----
        - The function removes the Unix permission bits when on Windows, as the permission bits are not
        supported on Windows.
        """
        # Map of file extensions to extraction methods
        extraction_methods = {
            (".tar.gz", ".tgz"): self.__extract_tar_archive,
            (".zip",): self.__extract_zip_archive,
        }

        # Find the appropriate extraction method
        for extensions, method in extraction_methods.items():
            if any(archive_path.endswith(ext) for ext in extensions):
                method(archive_path, target_path)
                return

        # If no suitable method found, raise error
        supported_formats = [ext for exts in extraction_methods.keys() for ext in exts]
        raise ValueError(f"Unsupported archive format. Supported formats: {', '.join(supported_formats)}")

    def __extract_tar_archive(self, archive_path: str, target_path: str) -> None:
        """Extract a tar.gz or tgz archive."""
        with tarfile.open(archive_path, "r:gz") as tar:
            for member in tar.getmembers():
                # Strip the Unix permission bits when on Windows
                if os.name == "nt":
                    member.mode = 0o644  # Default file permissions for Windows
                tar.extract(member, path=target_path)

    def __extract_zip_archive(self, archive_path: str, target_path: str) -> None:
        """Extract a zip archive."""
        with zipfile.ZipFile(archive_path, "r") as zip_ref:
            zip_ref.extractall(target_path)


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False)

    print("Starting artifact download and installation...")
    download_and_install_artifacts(context)
    print("Finished")
