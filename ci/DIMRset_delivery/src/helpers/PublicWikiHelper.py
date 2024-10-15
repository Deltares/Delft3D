from datetime import date
import os
import re
from typing import Tuple

from lib.Atlassian import Atlassian
from lib.TeamCity import TeamCity
from settings.atlassian_settings import *
from settings.teamcity_settings import *


class PublicWikiHelper(object):
    """
    Class responsible for updating the Deltares Public Wiki for a specific DIMR version.
    """

    def __init__(self, atlassian: Atlassian, teamcity: TeamCity, dimr_version):
        """
        Creates a new instance of PublicWikiHelper.

        Args:
            atlassian (Atlassian): A reference to a Atlassian Confluence REST API wrapper.
            teamcity (TeamCity): A reference to a TeamCity REST API wrapper.
            dimr_version (str): The version of DIMR to update the Public Wiki for.
            svn_revision_number (str): The SVN revision of the DIMR set.
        """
        self.__atlassian = atlassian
        self.__teamcity = teamcity
        self.__dimr_version = dimr_version

        major, minor, patch = dimr_version.split('.')
        self.__major_version = major
        self.__minor_version = minor
        self.__patch_version = patch

    def update_public_wiki(self) -> None:
        """
        Creates and/or updates the Public Wiki for a specific DIMR version.

        Returns:
            None
        """
        print("Updating main wiki page...")
        main_page_id = self.__update_main_page()

        print("Updating sub wiki page...")
        self.__update_sub_page(parent_page_id=main_page_id)

    def __update_main_page(self) -> str:
        """
        Updates the content of the main page for the given DIMR version.

        Returns:
            str: The id of the updated page.
        """
        content = self.__prepare_content_for_main_page()
        page_to_update_page_id = self.__get_main_page_id_for_page_to_update()
        page_title = f"{DIMR_PATCH_PAGE_PREFIX} {self.__major_version}.{self.__minor_version}.{self.__patch_version}"
        self.__update_content_of_page(page_to_update_page_id, page_title, content)

        return page_to_update_page_id

    def __prepare_content_for_main_page(self) -> str:
        """
        Prepares the content that should be uploaded to the main page.

        Returns:
            str: The content.
        """
        windows_version_artifact, linux_version_artifact = self.__get_version_artifacts()
        if windows_version_artifact is None:
            raise AssertionError("Could not retrieve the Windows version.txt artifact.")
        if linux_version_artifact is None:
            raise AssertionError("Could not retrieve the Linux version.txt artifact.")

        current_number_unsigned_files, current_number_total_files = self.__get_number_of_files_for_latest_build()
        previous_number_unsigned_files, previous_number_total_files = self.__get_number_of_files_for_previous_build()

        content = self.__create_content_from_template(windows_version_artifact=windows_version_artifact,
                                                      linux_version_artifact=linux_version_artifact,
                                                      current_number_unsigned_files=current_number_unsigned_files,
                                                      current_number_total_files=current_number_total_files,
                                                      previous_number_unsigned_files=previous_number_unsigned_files,
                                                      previous_number_total_files=previous_number_total_files)

        return content

    def __get_version_artifacts(self) -> Tuple[str, str]:
        """
        Gets the latest Windows and Linux Version.txt artifacts from Dimr Collector Release.

        Returns:
            Tuple[str, str]: A tuple containing the Windows and Linux artifacts respectively.
        """
        latest_dimr_collector_release_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID)

        windows_version_artifact = self.__teamcity.get_build_artifact(build_id=latest_dimr_collector_release_build_id,
                                                                      path_to_artifact=PATH_TO_WINDOWS_VERSION_ARTIFACT)

        linux_version_artifact = self.__teamcity.get_build_artifact(build_id=latest_dimr_collector_release_build_id,
                                                                    path_to_artifact=PATH_TO_LINUX_VERSION_ARTIFACT)

        return windows_version_artifact.decode(), linux_version_artifact.decode()

    def __get_number_of_files_for_latest_build(self) -> Tuple[str, str]:
        """
        Gets the number of unsigned files and number of total files for the latest DIMR build.

        Returns:
            Tuple[str, str]: The number of unsigned files and the number of total files respectively.
        """
        summary_artifact = self.__get_summary_artifact()
        if summary_artifact is None:
            raise AssertionError("Could not retrieve the Summary.txt artifact of the latest build.")
        return self.__extract_file_numbers_from_summary_artifact(summary_artifact=summary_artifact)

    def __get_summary_artifact(self) -> str:
        """
        Gets the Summary.txt artifact from the Dimr Collector Release Signed build.

        Returns:
            str: The Summary.txt artifact.
        """
        latest_dimr_collector_release_signed_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID)

        summary_artifact = self.__teamcity.get_build_artifact(build_id=latest_dimr_collector_release_signed_build_id,
                                                              path_to_artifact=PATH_TO_DIMR_COLLECTOR_RELEASE_SIGNED_ARTIFACT)
        return summary_artifact.decode()

    def __extract_file_numbers_from_summary_artifact(self, summary_artifact: str) -> Tuple[str, str]:
        """
        The summary artifact will always have two numbers. The first number is the number of unsigned files.
        The second number is the number of total files.
        """
        numbers = re.findall(r"\d+", summary_artifact)
        number_unsigned_files = numbers[0]
        number_total_files = numbers[1]
        return number_unsigned_files, number_total_files

    def __get_number_of_files_for_previous_build(self) -> Tuple[str, str]:
        """
        Gets the number of unsigned files and number of total files for the previous pinned DIMR build.

        Returns:
            Tuple[str, str]: The number of unsigned files and the number of total files respectively.
        """
        latest_dimr_collector_release_signed_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID)

        dimr_collector_release_builds = self.__teamcity.get_builds_for_build_type_id(
            build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID,
            limit=2,
            include_failed_builds=False,
            pinned="true")
        if len(dimr_collector_release_builds["build"]) == 0:
            # Probably a new TeamCity
            return ("notFound", "notFound")

        # it could be the that the latest build has already been pinned, in that case take the second build returned
        if dimr_collector_release_builds["build"][0]["id"] != latest_dimr_collector_release_signed_build_id:
            previous_build_id = dimr_collector_release_builds["build"][0]["id"]
        else:
            previous_build_id = dimr_collector_release_builds["build"][1]["id"]

        previous_summary_artifact = \
            self.__teamcity.get_build_artifact(build_id=previous_build_id,
                                               path_to_artifact=PATH_TO_DIMR_COLLECTOR_RELEASE_SIGNED_ARTIFACT)
        if previous_summary_artifact is None:
            raise AssertionError("Could not retrieve the Summary.txt artifact of the previous build.")

        return self.__extract_file_numbers_from_summary_artifact(previous_summary_artifact.decode())

    def __create_content_from_template(self, windows_version_artifact: str, linux_version_artifact: str,
                                       current_number_unsigned_files: str, current_number_total_files: str,
                                       previous_number_unsigned_files: str, previous_number_total_files: str) -> str:
        """
        Creates the content for the main DIMR page from a template file.

        Args:
            windows_version_artifact (str): The Windows Version.txt artifact.
            linux_version_artifact (str): The Linux Version.txt artifact.
            current_number_unsigned_files (str): The number of unsigned files for the current DIMR build.
            current_number_total_files (str): The total number of files for the current DIMR build.
            previous_number_unsigned_files (str): The number of unsigned files for the previous DIMR build.
            previous_number_total_files (str): The total number of files for the previous DIMR build.

        Returns:
            str: The content.
        """
        current_dir = os.path.dirname(__file__)
        path_to_wiki_template = os.path.join(current_dir, RELATIVE_PATH_TO_WIKI_TEMPLATE)

        with open(path_to_wiki_template, 'r') as file:
            data = file.read()

        data = data.replace("@@@DATE@@@", date.today().strftime("%d-%m-%Y"))
        data = data.replace("@@@WINDOWS_VERSION_ARTIFACT@@@", f"<pre>{windows_version_artifact}</pre>")
        data = data.replace("@@@LINUX_VERSION_ARTIFACT@@@", f"<pre>{linux_version_artifact}</pre>")
        data = data.replace("@@@DIMR_RELEASE_VERSION@@@",
                            f"{self.__major_version}.{self.__minor_version}.{self.__patch_version}")
        data = data.replace("@@@CURRENT_UNSIGNED_FILES@@@", current_number_unsigned_files)
        data = data.replace("@@@CURRENT_TOTAL_FILES@@@", current_number_total_files)
        data = data.replace("@@@PREVIOUS_UNSIGNED_FILES@@@", previous_number_unsigned_files)
        data = data.replace("@@@PREVIOUS_TOTAL_FILES@@@", previous_number_total_files)

        return data

    def __get_main_page_id_for_page_to_update(self) -> str:
        """
        Gets the page id for the Public Wiki page we want to update.

        This method first checks if there is a page for the current major version of DIMR on the root DIMR page.
        If there is no such page, it will be created. (e.g. DIMRset 2)

        It will then check if there is a page for the current major.minor version of DIMR under the major version page.
        If there is no such page, it will be created. (e.g. DIMRset 2 -> DIMRset 2.13)

        It will then check if there is a page for the current major.minor.patch version of DIMR under the major.minor
        version page.
        If there is no such page, it will be created. (e.g. DIMRset 2 -> DIMRset 2.13 -> DIMRset 2.13.03)

        It will then return the page id for the major.minor.patch page. This is the page that should be updated
        (so: the id of the DIMRset 2.13.03 page).

        Returns:
            str: The page id of the page to be updated.
        """
        dimr_major_version_page_id = self.__get_public_wiki_page_id(parent_page_id=DIMR_ROOT_PAGE_ID,
                                                                    dimr_version=self.__major_version,
                                                                    prefix=DIMR_MAJOR_PAGE_PREFIX)

        dimr_minor_version_page_id = \
            self.__get_public_wiki_page_id(parent_page_id=dimr_major_version_page_id,
                                           dimr_version=f"{self.__major_version}.{self.__minor_version}",
                                           prefix=DIMR_MINOR_PAGE_PREFIX)

        dimr_patch_version_page_id = \
            self.__get_public_wiki_page_id(parent_page_id=dimr_minor_version_page_id,
                                           dimr_version=f"{self.__major_version}.{self.__minor_version}.{self.__patch_version}",
                                           prefix=DIMR_PATCH_PAGE_PREFIX)

        return dimr_patch_version_page_id

    def __get_public_wiki_page_id(self, parent_page_id: str, dimr_version: str, prefix: str, suffix: str = "") -> str:
        """
        Checks if there is already a page for the specified DIMR version
        under the specified parent page. If there is no such page, it
        will be created.

        Returns:
            str: The id for the page of the specified DIMR version.
        """
        parent_page = self.__atlassian.get_page_info_for_parent_page(parent_page_id=parent_page_id)
        page_exists = False
        page_id = ""

        for result in parent_page["results"]:
            if "title" in result and result["title"] == f"{prefix} {dimr_version}{suffix}":
                page_exists = True
                page_id = result["id"]
                break

        if not page_exists:
            page_title = f"{prefix} {dimr_version}{suffix}"
            page_id = self.__atlassian.create_public_wiki_page(page_title=page_title, space_id=DIMR_SPACE_ID,
                                                               ancestor_id=parent_page_id)

        if page_id == "" or page_id is None:
            raise AssertionError(f"Could not find or create the page for {prefix} {dimr_version}{suffix}.")

        return page_id

    def __update_content_of_page(self, page_id: str, page_title: str, content: str) -> None:
        """
        Updates the page with the given page id on the Public Wiki with the given title and content.

        Args:
            page_id (str): The id for the page to update.
            page_title (str): The title for the page to update.
            content (str): The content for the page to update.

        Returns:
            None
        """
        page_updated_successfully = self.__atlassian.update_page(page_id=page_id,
                                                                 page_title=page_title,
                                                                 content=content)
        if not page_updated_successfully:
            raise AssertionError("Failed to update the public wiki page.")

    def __update_sub_page(self, parent_page_id) -> None:
        """
        Updates the sub page for the given DIMR version.
        Args:
            parent_page_id (str): The id for the main page of the specified DIMR version.

        Returns:
            None
        """
        content = self.__prepare_content_for_sub_page()

        subpage_to_update_page_id = self.__get_public_wiki_page_id(parent_page_id=parent_page_id,
                                                                   dimr_version=self.__dimr_version,
                                                                   prefix=DIMR_SUBPAGE_PREFIX,
                                                                   suffix=DIMR_SUBPAGE_SUFFIX)
        page_title = f"{DIMR_SUBPAGE_PREFIX} {self.__dimr_version}{DIMR_SUBPAGE_SUFFIX}"
        self.__update_content_of_page(page_id=subpage_to_update_page_id,
                                      page_title=page_title, content=content)

    def __prepare_content_for_sub_page(self) -> str:
        """
        Prepares the content that should be uploaded to the sub page.

        Returns:
            str: The content.
        """
        latest_dimr_release_testbench_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID)
        release_testbench_artifact = self.__teamcity.get_build_artifact(latest_dimr_release_testbench_build_id,
                                                                        PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT)

        # decode bytes and add the <pre> ... </pre> tags to make sure the wiki page properly keeps the formatting
        content = f"<pre>{release_testbench_artifact.decode()}</pre>"

        return content
