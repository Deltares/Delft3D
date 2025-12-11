#!/usr/bin/env python3
"""Download the artifacts and install them on Linux machine."""

import os
import shutil
import sys
import tarfile
import zipfile
from typing import List, Optional

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class ArtifactInstaller(StepExecutorInterface):
    """
    Executes the step to download and install build artifacts on a Linux machine.

    This class retrieves artifacts from TeamCity, deploys them to a remote system via SSH,
    and performs installation tasks as part of the DIMR automation workflow.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing configuration and state.
    services : Services
        The collection of external service clients required for artifact retrieval and deployment (e.g., TeamCity, SSH).
    remote_base_path : str, optional
        Base path for DIMR installation on remote system.
    installation_path : str, optional
        Installation subdirectory (e.g., 'weekly', 'release').

    Usage
    -----
    Instantiate and call `execute_step()` to perform the artifact download and installation process.
    """

    def __init__(
        self,
        context: DimrAutomationContext,
        services: Services,
        remote_base_path: str = "/p/d-hydro/dimrset",
        installation_path: str = "weekly",
    ) -> None:
        """
        Initialize ArtifactInstaller.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing configuration and state.
        services : Services
            The collection of external service clients required for artifact retrieval and deployment.
        remote_base_path : str, optional
            Base path for DIMR installation on remote system.
        installation_path : str, optional
            Installation subdirectory (e.g., 'weekly', 'release').
        """
        self.context = context
        self.services = services
        self.__dimr_version = context.dimr_version
        self.__branch_name = context.branch_name
        self.__remote_base_path = remote_base_path
        self.__installation_path = installation_path
        self.__teamcity = services.teamcity
        self.__ssh_client = services.ssh

    def execute_step(self) -> bool:
        """
        Download the artifacts and install them on the Linux machine.

        Returns
        -------
        bool
            True if successful, False otherwise.

        Raises
        ------
        ValueError
            If required clients are not initialized.
        """
        self.context.log("Downloading and installing artifacts...")

        if self.context.dry_run:
            self.context.log(f"Download artifacts for TeamCity build ID: {self.context.build_id}")
            self.context.log("Publish downloaded artifacts to the designated network drive location")
            self.context.log("Publish the weekly DIMR build via H7 deployment process")
            return True

        if self.services.teamcity is None:
            self.context.log("TeamCity client is required but not initialized", severity=LogLevel.ERROR)
            return False
        if self.services.ssh is None:
            self.context.log("SSH client is required but not initialized", severity=LogLevel.ERROR)
            return False

        self.__download_and_deploy_artifacts()
        self.__install_dimr_on_remote_system()

        self.context.log("Artifacts download and installation completed successfully!")
        return True

    def __download_and_deploy_artifacts(self) -> None:
        """Download the DIMR artifacts from TeamCity and deploy them to the remote system."""
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")
        if self.__ssh_client is None:
            raise ValueError("SSH client is required but not initialized")
        windows_collect_id = self.__teamcity.get_dependent_build_id(
            self.context.build_id, self.context.settings.teamcity_ids.delft3d_windows_collect_build_type_id
        )

        linux_collect_id = self.__teamcity.get_dependent_build_id(
            self.context.build_id, self.context.settings.teamcity_ids.delft3d_linux_collect_build_type_id
        )

        self.__download_and_deploy_artifact_by_name(
            str(windows_collect_id) if windows_collect_id is not None else "",
            self.context.settings.name_of_dimr_release_signed_windows_artifact,
        )

        self.__download_and_deploy_artifact_by_name(
            str(linux_collect_id) if linux_collect_id is not None else "",
            self.context.settings.name_of_dimr_release_signed_linux_artifact,
            exclude_directories=["lnx64/test"],
        )

    def __install_dimr_on_remote_system(self) -> None:
        """Install DIMR on the Linux machine via SSH."""
        if self.__ssh_client is None:
            raise ValueError("SSH client is required but not initialized")
        print(f"Installing DIMR on {self.context.settings.linux_address} via SSH...")

        command = self.__build_ssh_installation_command()
        self.__ssh_client.execute(command=command)

    def __build_ssh_installation_command(self) -> str:
        """
        Build the SSH command for DIMR installation.

        Returns
        -------
        str
            The SSH command string for installation.
        """
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

    def __download_and_deploy_artifact_by_name(
        self, build_id: str, artifact_name_key: str, exclude_directories: Optional[List[str]] = None
    ) -> None:
        """
        Download and unpack artifacts from a TeamCity build that match the specified artifact name key.

        Parameters
        ----------
        build_id : str
            The ID of the TeamCity build to retrieve artifacts from.
        artifact_name_key : str
            Substring to filter artifact names for downloading.
        exclude_directories : Optional[List[str]]
            List of directory names to exclude from deploying.

        Raises
        ------
        ValueError
            If artifact names cannot be retrieved.
        """
        if exclude_directories is None:
            exclude_directories = []
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")
        artifact_names = self.__teamcity.get_build_artifact_names(build_id=build_id)
        if artifact_names is None:
            raise ValueError(f"Could not retrieve artifact names for build {build_id}")
        artifacts_to_download = [a["name"] for a in artifact_names["file"] if artifact_name_key in a["name"]]
        self.__download_and_unpack_dimr_artifacts(
            artifacts_to_download=artifacts_to_download, build_id=build_id, exclude_directories=exclude_directories
        )

    def __download_and_unpack_dimr_artifacts(
        self, artifacts_to_download: List[str], build_id: str, exclude_directories: Optional[List[str]]
    ) -> None:
        """
        Download the provided artifact names from the specified TeamCity build ID.

        Parameters
        ----------
        artifacts_to_download : List[str]
            A list of artifact names to download.
        build_id : str
            The build ID for the build to download the artifacts from.
        exclude_directories : Optional[List[str]]
            List of directory names to exclude from deploying.
        """
        self.__ensure_version_directory_exists()

        for artifact_name in artifacts_to_download:
            self.__download_extract_and_deploy_artifact(artifact_name, build_id, exclude_directories)

    def __ensure_version_directory_exists(self) -> None:
        """Ensure the version directory exists for artifact storage."""
        file_path = f"{self.__dimr_version}"
        if not os.path.exists(file_path):
            os.makedirs(file_path)

    def __download_extract_and_deploy_artifact(
        self, artifact_name: str, build_id: str, exclude_directories: Optional[List[str]]
    ) -> None:
        """
        Download, extract, and deploy a single artifact.

        Parameters
        ----------
        artifact_name : str
            The name of the artifact to download.
        build_id : str
            The build ID to download the artifact from.
        exclude_directories : Optional[List[str]]
            List of directory names to exclude from deploying.

        Raises
        ------
        ValueError
            If the artifact cannot be downloaded.
        """
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")
        if self.__ssh_client is None:
            raise ValueError("SSH client is required but not initialized")
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

        # Clean up the downloaded archive
        if exclude_directories is None:
            exclude_directories = []
        for exclude_directory in exclude_directories:
            directory = os.path.join(self.__dimr_version, exclude_directory)
            if os.path.exists(directory):
                print(f"Removing {directory}...")
                shutil.rmtree(directory)

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
        archive_path : str
            The path to the archive (tar or zip).
        target_path : str
            The path to extract the archive to.

        Raises
        ------
        ValueError
            If the archive format is unsupported.
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
        """
        Extract a tar.gz or tgz archive.

        Parameters
        ----------
        archive_path : str
            The path to the tar archive.
        target_path : str
            The path to extract the archive to.
        """
        with tarfile.open(archive_path, "r:gz") as tar:
            for member in tar.getmembers():
                # Strip the Unix permission bits when on Windows
                if os.name == "nt":
                    member.mode = 0o644  # Default file permissions for Windows
                tar.extract(member, path=target_path)

    def __extract_zip_archive(self, archive_path: str, target_path: str) -> None:
        """
        Extract a zip archive.

        Parameters
        ----------
        archive_path : str
            The path to the zip archive.
        target_path : str
            The path to extract the archive to.
        """
        with zipfile.ZipFile(archive_path, "r") as zip_ref:
            zip_ref.extractall(target_path)


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_git=False, require_jira=False)
        services = Services(context)

        context.log("Starting deploying artifacts...")
        if ArtifactInstaller(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Deploying artifacts failed!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nDeploying artifacts interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Deploying artifacts failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
