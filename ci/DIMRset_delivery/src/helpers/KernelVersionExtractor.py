import re
from typing import Any, Dict

from settings.general_settings import DRY_RUN_PREFIX
from lib.TeamCity import TeamCity
from settings.teamcity_settings import KERNELS, TEAMCITY_IDS


class KernelVersionExtractor(object):
    """
    Class responsible for extracting the correct kernel versions from
    the latest successful DIMR build. The values that are extracted are
    the revision numbers that have been entered manually before starting
    the build.
    """

    def __init__(self, teamcity: TeamCity):
        """
        Creates a new instance of KernelVersionExtractor.

        Arguments:
             teamcity (TeamCity): A reference to a TeamCity REST API wrapper.
        """
        self.__teamcity = teamcity

    def get_latest_kernel_versions(self, teamcity: TeamCity, build_id_chain: str, dry_run: bool) -> Dict[str, str]:
        """
        Gets the kernel versions from the latest Dimr Collector Release build.

        Returns:
            Dict[str, str]: A dictionary of "kernel name" -> "version"
        """
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Get build info of build_id {build_id_chain}, then extract kernel versions from properties.")
            kernel_versions = {
                KERNELS[0].name_for_extracting_revision: "1.23.45",
                KERNELS[1].name_for_extracting_revision: "abcdefghijklmnopqrstuvwxyz01234567890123"
            }
        else:
            publish_build_info = teamcity.get_build_info_for_build_id(build_id_chain)
            kernel_versions = self.__extract_kernel_versions(
                build_info=publish_build_info
            )
        return kernel_versions

    def assert_all_versions_have_been_extracted(self, kernel_versions: Dict[str, str]) -> None:
        """ Asserts all expected kernels have had their version extracted. """
        missing_kernel_versions = []
        for kernel in kernel_versions:
            if kernel_versions[kernel] is None:
                missing_kernel_versions.append(kernel)
            else:
                print(f"Found {kernel}: #{kernel_versions[kernel]}")
        if len(missing_kernel_versions) == 0:
            print("All kernel revision numbers have successfully been found.")
            return

        error = "Could not find the revision number for the following kernels: \n"
        error += ', '.join(missing_kernel_versions)
        raise AssertionError(error)

    def get_branch_name(self, teamcity: TeamCity, build_id_chain: str, dry_run: bool) -> str:
        """Returns the branch name from the latest release collector build."""
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Get build info of build_id {build_id_chain}, then get branch name from properties.")
            self.__branch_name = "main"
            print(f"{DRY_RUN_PREFIX} simulating '{self.__branch_name}' branch")
            return self.__branch_name
        latest_publish_build_info = teamcity.get_build_info_for_build_id(
            build_id_chain
        )

        branch_name_property = next(
            (
                prop
                for prop in latest_publish_build_info["resultingProperties"]["property"]
                if prop["name"] == "teamcity.build.branch"
            ),
            None,
        )
        self.__branch_name = branch_name_property["value"]
        return self.__branch_name

    def get_dimr_version(self, kernel_versions: Dict[str, str]) -> str:
        """ Extracts and returns the DIMR version that requires automated release. """
        if kernel_versions is None:
            raise AssertionError("Could not extract the DIMR version: the kernel versions have not yet been extracted")
        dimr_version = kernel_versions["DIMRset_ver"]
        return dimr_version

    def __extract_kernel_versions(self, build_info: Dict[str, Any]) -> Dict[str, str]:
        """
        Extracts the kernel versions that have been entered manually from the provided build info.

        Args:
            build_info (Dict[str, Any]: The build info as provided by the TeamCity API wrapper.

        Returns:
            Dict[str, str]: A dictionary mapping a kernel name to a kernel version.
        """
        kernel_versions = {}
        for KERNEL in KERNELS:
            kernel_versions[KERNEL.name_for_extracting_revision] = None

        for kernel in build_info["resultingProperties"]["property"]:
            if any(KERNEL.name_for_extracting_revision == kernel["name"] for KERNEL in KERNELS):
                kernel_versions[kernel["name"]] = kernel["value"]

        return kernel_versions
