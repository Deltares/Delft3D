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
from settings.general_settings import (
    DRY_RUN_PREFIX,
    DRY_RUN_PREFIX,
    LINUX_ADDRESS,
    VERSIONS_EXCEL_FILENAME,
)
from settings.teamcity_settings import (
    PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT,
    TEAMCITY_IDS,
)
from typing import Dict


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

    def run(self, build_id_chain: str, dry_run: bool = False) -> None:
        """Runs the actual DIMR release automation steps."""
        self.__assert_preconditions(dry_run)

        extractor = KernelVersionExtractor(self.__teamcity)

        branch_name = extractor.get_branch_name(self.__teamcity, build_id_chain, dry_run)
        kernel_versions = extractor.get_latest_kernel_versions(self.__teamcity, build_id_chain, dry_run)
        dimr_version = extractor.get_dimr_version(kernel_versions)
        extractor.assert_all_versions_have_been_extracted(kernel_versions)

        self.__download_and_install_artifacts(build_id_chain, dimr_version, branch_name,dry_run)
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would tag commit with:", 
                  f"commit={kernel_versions['build.vcs.number']}, tag=DIMRset_{dimr_version}")
        else:
            self.__git_client.tag_commit(
                kernel_versions["build.vcs.number"], f"DIMRset_{dimr_version}"
            )
        self.__pin_and_tag_builds(build_id_chain, dimr_version, dry_run)
        self.__update_excel_sheet(kernel_versions, dimr_version, dry_run)
        self.__prepare_email(build_id_chain, kernel_versions, dimr_version, dry_run)
        self.__update_public_wiki(build_id_chain, dimr_version, dry_run)

    def __assert_preconditions(self, dry_run: bool) -> None:
        """Asserts some preconditions are met before the script is fully run."""
        preconditions = PreconditionsHelper(
            atlassian=self.__atlassian,
            teamcity=self.__teamcity,
            ssh_client=self.__ssh_client,
            git_client=self.__git_client,
        )
        preconditions.assert_preconditions(dry_run)

    def __update_public_wiki(self, build_id_chain: str, dimr_version: str, dry_run: bool) -> None:
        """Updates the Public Wiki."""
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would update public wiki for DIMR version:", dimr_version)
            return
        print("Updating the public wiki...")
        public_wiki = PublicWikiHelper(
            atlassian=self.__atlassian,
            teamcity=self.__teamcity,
            dimr_version=dimr_version,
        )
        public_wiki.update_public_wiki(build_id_chain)

    def __download_and_install_artifacts(self, build_id_chain: str, dimr_version: str, branch_name: str, dry_run: bool) -> None:
        """Downloads the artifacts and installs them on Linux machine."""
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would download artifacts for build from TeamCity:", build_id_chain)
            print(f"{DRY_RUN_PREFIX} Would publish artifacts to network drive")
            print(f"{DRY_RUN_PREFIX} Would publish weekly DIMR via H7")
            return
        helper = ArtifactInstallHelper(
            teamcity=self.__teamcity,
            ssh_client=self.__ssh_client,
            dimr_version=dimr_version,
            branch_name=branch_name,
        )
        helper.publish_artifacts_to_network_drive(build_id_chain)
        helper.publish_weekly_dimr_via_h7()

    def __pin_and_tag_builds(self, build_id_chain: str, dimr_version: str, dry_run: bool) -> None:
        """Pin and tag the appropriate builds."""
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would pin and tag builds in TeamCity for build chain:", build_id_chain)
            print(f"{DRY_RUN_PREFIX} Would add tag:", f"DIMRset_{dimr_version}")
            return
        helper = PinHelper(teamcity=self.__teamcity, dimr_version=dimr_version)
        helper.pin_and_tag_builds(build_id_chain)

    def __update_excel_sheet(self, kernel_versions: Dict[str, str], dimr_version: str, dry_run: bool) -> None:
        """Updates the Excel sheet with this week's release information."""
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would update Excel sheet with DIMR version:", dimr_version)
            print(f"{DRY_RUN_PREFIX} Would download Excel from network drive")
            print(f"{DRY_RUN_PREFIX} Would append new row with release information")
            print(f"{DRY_RUN_PREFIX} Would upload updated Excel back to network drive")
            return
        parser = self.__get_testbank_result_parser()
        path_to_excel_file = f"/p/d-hydro/dimrset/{VERSIONS_EXCEL_FILENAME}"

        self.__ssh_client.secure_copy(
            LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.FROM
        )
        helper = ExcelHelper(
            teamcity=self.__teamcity,
            filepath=VERSIONS_EXCEL_FILENAME,
            dimr_version=dimr_version,
            kernel_versions=kernel_versions,
            parser=parser,
        )
        helper.append_row()
        self.__ssh_client.secure_copy(
            LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.TO
        )

    def __prepare_email(self, build_id_chain: str, kernel_versions: Dict[str, str], dimr_version: str, dry_run: bool) -> None:
        if dry_run:
            print(f"{DRY_RUN_PREFIX} Would prepare email template for DIMR version:", dimr_version)
            return
        parser = self.__get_testbank_result_parser()
        previous_parser = self.__get_previous_testbank_result_parser(build_id_chain)

        helper = EmailHelper(
            dimr_version=dimr_version,
            kernel_versions=kernel_versions,
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
