from helpers.ArtifactInstallHelper import ArtifactInstallHelper
from helpers.EmailHelper import EmailHelper
from helpers.ExcelHelper import ExcelHelper
from helpers.GitClient import GitClient
from helpers.KernelVersionExtractor import KernelVersionExtractor
from helpers.PinHelper import PinHelper
from helpers.PreconditionsHelper import PreconditionsHelper
from helpers.PublicWikiHelper import PublicWikiHelper
from helpers.SshClient import Direction, SshClient
from helpers.TestbankResultParser import TestbankResultParser
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME
from settings.teamcity_settings import (
    PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT,
    TEAMCITY_IDS,
)


class DimrAutomation(object):
    """
    Class to automate the latest weekly DIMR release. This includes
    updating the public wiki, downloading the artifacts to the network drive,
    installing the new DIMR set on Linux, pinning and tagging the appropriate builds,
    updating SVN, and preparing a mail for the release notification.
    """

    def __init__(
        self,
        atlassian: Atlassian,
        teamcity: TeamCity,
        ssh_client: SshClient,
        git_client: GitClient,
    ):
        """
        Creates a new instance of DimrAutomation.

        Args:
            atlassian (Atlassian): A wrapper for the Atlassian Confluence REST API.
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            ssh_client: A wrapper for a SSH client.
            git_client: A wrapper for a git client.
        """
        self.__atlassian = atlassian
        self.__teamcity = teamcity
        self.__ssh_client = ssh_client
        self.__git_client = git_client

        self.__branch_name = None
        self.__kernel_versions = None
        self.__dimr_version = None

    def run(self, build_id_chain: str) -> None:
        """Runs the actual DIMR release automation steps."""
        self.__assert_preconditions()
        # __get_kernel_versions is crucial for the script to run, do not comment this one out!
        self.__get_kernel_versions(build_id_chain)
        self.__download_and_install_artifacts(build_id_chain)
        self.__git_client.tag_commit(
            self.__kernel_versions["build.vcs.number"], f"DIMRset_{self.__dimr_version}"
        )
        self.__pin_and_tag_builds(build_id_chain)
        self.__update_excel_sheet()
        self.__prepare_email(build_id_chain)  # depending on TC tags
        self.__update_public_wiki(build_id_chain)

    def __assert_preconditions(self) -> None:
        """Asserts some preconditions are met before the script is fully run."""
        preconditions = PreconditionsHelper(
            atlassian=self.__atlassian,
            teamcity=self.__teamcity,
            ssh_client=self.__ssh_client,
            git_client=self.__git_client,
        )
        preconditions.assert_preconditions()

    def __get_kernel_versions(self, build_id_chain: str) -> None:
        """
        Extract and set the kernel versions based on the information that has been manually
        set in the TeamCity build settings.
        """
        extractor = KernelVersionExtractor(teamcity=self.__teamcity)

        self.__branch_name = extractor.get_branch_name(build_id_chain)

        self.__kernel_versions = extractor.get_latest_kernel_versions(build_id_chain)
        extractor.assert_all_versions_have_been_extracted()

        self.__dimr_version = extractor.get_dimr_version()

    def __update_public_wiki(self, build_id_chain: str) -> None:
        """Updates the Public Wiki."""
        print("Updating the public wiki...")
        public_wiki = PublicWikiHelper(
            atlassian=self.__atlassian,
            teamcity=self.__teamcity,
            dimr_version=self.__dimr_version,
        )
        public_wiki.update_public_wiki(build_id_chain)

    def __download_and_install_artifacts(self, build_id_chain: str) -> None:
        """Downloads the artifacts and installs them on Linux machine."""
        helper = ArtifactInstallHelper(
            teamcity=self.__teamcity,
            ssh_client=self.__ssh_client,
            dimr_version=self.__dimr_version,
            branch_name=self.__branch_name,
        )
        helper.publish_artifacts_to_network_drive(build_id_chain)
        helper.publish_weekly_dimr_via_h7()

    def __pin_and_tag_builds(self, build_id_chain: str) -> None:
        """Pin and tag the appropriate builds."""
        helper = PinHelper(teamcity=self.__teamcity, dimr_version=self.__dimr_version)
        helper.pin_and_tag_builds(build_id_chain)

    def __update_excel_sheet(self) -> None:
        """Updates the Excel sheet with this week's release information."""
        parser = self.__get_testbank_result_parser()
        path_to_excel_file = f"/p/d-hydro/dimrset/{VERSIONS_EXCEL_FILENAME}"

        self.__ssh_client.secure_copy(
            LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.FROM
        )
        helper = ExcelHelper(
            teamcity=self.__teamcity,
            filepath=VERSIONS_EXCEL_FILENAME,
            dimr_version=self.__dimr_version,
            kernel_versions=self.__kernel_versions,
            parser=parser,
        )
        helper.append_row()
        self.__ssh_client.secure_copy(
            LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.TO
        )

    def __prepare_email(self, build_id_chain: str) -> None:
        parser = self.__get_testbank_result_parser()
        previous_parser = self.__get_previous_testbank_result_parser(build_id_chain)

        helper = EmailHelper(
            dimr_version=self.__dimr_version,
            kernel_versions=self.__kernel_versions,
            current_parser=parser,
            previous_parser=previous_parser,
        )
        helper.generate_template()

    def __get_testbank_result_parser(self) -> TestbankResultParser:
        """Gets a new TestbankResultParser for the latest test bench results from a local file."""
        with open(PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT, "rb") as f:
            artifact = f.read()
        return TestbankResultParser(artifact.decode())

    def __get_previous_testbank_result_parser(
        self, build_id_chain: str
    ) -> TestbankResultParser:
        """Gets a new TestbankResultParser for the previous versioned tagged test bench results."""

        current_build_info = self.__teamcity.get_full_build_info_for_build_id(
            build_id_chain
        )
        build_type_id = current_build_info.get("buildTypeId")
        current_tag_name = self.__get_tag_from_build_info(current_build_info)

        # Get all builds for the publish build type
        latest_builds = self.__teamcity.get_builds_for_build_type_id(
            build_type_id=build_type_id,
            limit=50,
            include_failed_builds=False,
        )

        if latest_builds is None:
            latest_builds = []

        previous_build_id = None
        previous_version = None

        # Find previous versioned tagged build (major.minor.patch < current)
        for build in latest_builds.get("build", {}):
            build_id = build.get("id")
            if build_id == int(build_id_chain):
                continue

            loop_build_info = self.__teamcity.get_full_build_info_for_build_id(build_id)
            loop_tag_name = self.__get_tag_from_build_info(loop_build_info)

            if (
                loop_tag_name
                and loop_tag_name != (0, 0, 0)
                and current_tag_name
                and loop_tag_name < current_tag_name
            ):
                if previous_version is None or loop_tag_name > previous_version:
                    previous_build_id = build_id
                    previous_version = loop_tag_name
        
        # Previous version not found
        if previous_build_id is None:
            return None  

        # Download artifact for previous build
        artifact = self.__teamcity.get_build_artifact(
            build_id=f"{previous_build_id}",
            path_to_artifact=PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT,
        )
        return TestbankResultParser(artifact.decode())

    def __get_tag_from_build_info(self, current_build_info):
        current_tag_name = (0, 0, 0)
        tags = current_build_info.get("tags", {}).get("tag", [])
        for tag in tags:
            tag_name = tag.get("name")
            if tag_name.startswith("DIMRset_"):
                current_tag_name = self.__parse_version(tag_name)
        return current_tag_name

    def __parse_version(self, tag):
        if tag and tag.startswith("DIMRset_"):
            return tuple(map(int, tag[len("DIMRset_") :].split(".")))
        return None
