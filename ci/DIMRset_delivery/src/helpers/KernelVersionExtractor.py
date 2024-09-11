import re
from typing import Any, Dict

from lib.TeamCity import TeamCity
from settings.teamcity_settings import DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID, KERNELS, DIMR_TO_NGHS_BUILD_TYPE_ID


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
        self.__kernel_versions: Dict[str, str] = None
        self.__dimr_version = None

    def get_latest_kernel_versions(self) -> Dict[str, str]:
        """
        Gets the kernel versions from the latest Dimr Collector Release build.

        Returns:
            Dict[str, str]: A dictionary of "kernel name" -> "version"
        """
        latest_dimr_collector_build_info = self.__teamcity.get_build_info_for_latest_build_for_build_type_id(
            build_type_id=DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID)
        self.__kernel_versions = self.__extract_kernel_versions(build_info=latest_dimr_collector_build_info)
        return self.__kernel_versions

    def assert_all_versions_have_been_extracted(self) -> None:
        """ Asserts all expected kernels have had their version extracted. """
        missing_kernel_versions = []
        for kernel in self.__kernel_versions:
            if self.__kernel_versions[kernel] is None:
                missing_kernel_versions.append(kernel)
            else:
                print(f"Found {kernel}: #{self.__kernel_versions[kernel]}")
        if len(missing_kernel_versions) == 0:
            print("All kernel revision numbers have successfully been found.")
            return

        error = "Could not find the revision number for the following kernels: \n"
        error += ', '.join(missing_kernel_versions)
        raise AssertionError(error)

    def get_dimr_version(self) -> str:
        """ Extracts and returns the DIMR version that requires automated release. """
        if self.__kernel_versions is None:
            raise AssertionError("Could not extract the DIMR version: the kernel versions have not yet been extracted")
        self.__dimr_version = self.__kernel_versions["DIMRset_ver"]
        return self.__dimr_version


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

        for kernel in build_info["properties"]["property"]:
            if any(KERNEL.name_for_extracting_revision == kernel["name"] for KERNEL in KERNELS):
                kernel_versions[kernel["name"]] = kernel["value"]

        return kernel_versions
