from helpers.ArtifactInstallHelper import ArtifactInstallHelper
from helpers.EmailHelper import EmailHelper
from helpers.ExcelHelper import ExcelHelper
from helpers.KernelVersionExtractor import KernelVersionExtractor
from helpers.PinHelper import PinHelper
from helpers.SshClient import SshClient
from helpers.GitClient import GitClient
from helpers.TestbankResultParser import TestbankResultParser
from helpers.PublicWikiHelper import PublicWikiHelper
from helpers.PreconditionsHelper import PreconditionsHelper
from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.general_settings import *
from settings.teamcity_settings import *

class DimrAutomation(object):
    """
    Class to automate the latest weekly DIMR release. This includes
    updating the public wiki, downloading the artifacts to the network drive,
    installing the new DIMR set on Linux, pinning and tagging the appropriate builds,
    updating SVN, and preparing a mail for the release notification.
    """

    def __init__(self, atlassian: Atlassian, teamcity: TeamCity, ssh_client: SshClient, git_client: GitClient):
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

        self.__kernel_versions = None
        self.__dimr_version = None
        self.__svn_revision_number = None
        self.__full_dimr_version = None

    def run(self) -> None:
        """ Runs the actual DIMR release automation steps. """
        self.__assert_preconditions()
        self.__get_kernel_versions()  # This step is crucial for the script to run, do not comment this one out!
        self.__update_public_wiki()
        self.__download_and_install_artifacts()
        self.__git_client.tag_commit(self.__kernel_versions["OSS_ver"], f"DIMRset_{self.__dimr_version}")
        self.__pin_and_tag_builds()
        self.__update_excel_sheet()
        self.__prepare_email()

    def __assert_preconditions(self) -> None:
        """ Asserts some preconditions are met before the script is fully run. """
        preconditions = PreconditionsHelper(atlassian=self.__atlassian, teamcity=self.__teamcity,
                                            ssh_client=self.__ssh_client, git_client=self.__git_client)
        preconditions.assert_preconditions()

    def __get_kernel_versions(self) -> None:
        """
        Extract and set the kernel versions based on the information that has been manually
        set in the TeamCity build settings.
        """
        extractor = KernelVersionExtractor(teamcity=self.__teamcity)

        self.__kernel_versions = extractor.get_latest_kernel_versions()
        extractor.assert_all_versions_have_been_extracted()

        self.__dimr_version = extractor.get_dimr_version()

        self.__svn_revision_number = extractor.get_svn_revision_for_dimr_to_nghs()
        print(f"Found SVN revision number #{self.__svn_revision_number}.")

        self.__full_dimr_version = extractor.get_full_dimr_version()
        print(f"Found full DIMR version {self.__full_dimr_version}.")

    def __update_public_wiki(self) -> None:
        """ Updates the Public Wiki. """
        print("Updating the public wiki...")
        public_wiki = PublicWikiHelper(atlassian=self.__atlassian, teamcity=self.__teamcity, dimr_version=self.__dimr_version,
                                       svn_revision_number=self.__svn_revision_number)
        public_wiki.update_public_wiki()

    def __download_and_install_artifacts(self) -> None:
        """ Downloads the artifacts and installs them on Linux machine. """
        helper = ArtifactInstallHelper(teamcity=self.__teamcity, ssh_client=self.__ssh_client,
                                       full_dimr_version=self.__full_dimr_version)
        helper.download_artifacts_to_network_drive()
        helper.install_dimr_on_linux()

    def __pin_and_tag_builds(self) -> None:
        """ Pin and tag the appropriate builds. """
        helper = PinHelper(teamcity=self.__teamcity, dimr_version=self.__dimr_version)
        helper.pin_and_tag_builds()

    def __update_excel_sheet(self) -> None:
        """ Updates the Excel sheet with this week's release information. """
        parser = self.__get_testbank_result_parser()
        path_to_excel_file = f"{NETWORK_BASE_PATH}..\\{VERSIONS_EXCEL_FILENAME}"
        helper = ExcelHelper(teamcity=self.__teamcity, filepath=path_to_excel_file, dimr_version=self.__dimr_version,
                             svn_revision_number=self.__svn_revision_number, kernel_versions=self.__kernel_versions,
                             parser=parser)
        helper.append_row()

    def __prepare_email(self) -> None:
        parser = self.__get_testbank_result_parser()
        previous_parser = self.__get_previous_testbank_result_parser()

        helper = EmailHelper(dimr_version=self.__dimr_version, svn_revision=self.__svn_revision_number,
                             kernel_versions=self.__kernel_versions, current_parser=parser,
                             previous_parser=previous_parser)
        helper.generate_template()

    def __get_testbank_result_parser(self) -> TestbankResultParser:
        """ Gets a new TestbankResultParser for the latest test bench results. """
        latest_test_bench_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID)
        artifact = self.__teamcity.get_build_artifact(latest_test_bench_build_id, PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT)
        return TestbankResultParser(artifact.decode())

    def __get_previous_testbank_result_parser(self) -> TestbankResultParser:
        """ Gets a new TestbankResultParser for the previous pinned test bench results. """
        latest_test_bench_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID)

        pinned_test_bench_builds = self.__teamcity.get_builds_for_build_type_id(
            build_type_id=DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID,
            limit=2,
            include_failed_builds=False,
            pinned="true")

        if pinned_test_bench_builds["build"][0]["id"] != latest_test_bench_build_id:
            previous_test_bench_build_id = pinned_test_bench_builds["build"][0]["id"]
        else:
            previous_test_bench_build_id = pinned_test_bench_builds["build"][1]["id"]

        artifact = self.__teamcity.get_build_artifact(previous_test_bench_build_id,
                                                      PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT)

        return TestbankResultParser(artifact.decode())
