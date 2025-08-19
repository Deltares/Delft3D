#!/usr/bin/env python3
"""Update the Public Wiki with the new DIMR release information."""

import os
import sys
from datetime import datetime, timezone
from typing import Tuple

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface


class WikiPublisher(StepExecutorInterface):
    """Class responsible for updating the Deltares Public Wiki for a specific DIMR version."""

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        """
        Create a new instance of WikiPublisher.

        Parameters
        ----------
        atlassian : Atlassian
            A reference to a Atlassian Confluence REST API wrapper.
        teamcity : TeamCity
            A reference to a TeamCity REST API wrapper.
        dimr_version : str
            The version of DIMR to update the Public Wiki for.
        """
        self.__atlassian = services.atlassian
        self.__teamcity = services.teamcity
        self.__context = context
        self.__dimr_version = context.dimr_version
        self.__major_version, self.__minor_version, self.__patch_version = self.__dimr_version.split(".")

        self.__delft3d_windows_collect_build_type_id = (
            context.settings.teamcity_ids.delft3d_windows_collect_build_type_id
        )
        self.__delft3d_linux_collect_build_type_id = context.settings.teamcity_ids.delft3d_linux_collect_build_type_id
        self.__path_to_release_test_results_artifact = context.settings.path_to_release_test_results_artifact
        self.__path_to_windows_version_artifact = context.settings.path_to_windows_version_artifact
        self.__path_to_linux_version_artifact = context.settings.path_to_linux_version_artifact
        self.__relative_path_to_wiki_template = context.settings.relative_path_to_wiki_template
        self.__dimr_root_page_id = context.settings.dimr_root_page_id
        self.__dimr_major_page_prefix = context.settings.dimr_major_page_prefix
        self.__dimr_minor_page_prefix = context.settings.dimr_minor_page_prefix
        self.__dimr_patch_page_prefix = context.settings.dimr_patch_page_prefix
        self.__dimr_space_id = context.settings.dimr_space_id
        self.__dimr_subpage_prefix = context.settings.dimr_subpage_prefix
        self.__dimr_subpage_suffix = context.settings.dimr_subpage_suffix
        self.__build_id = context.build_id

    def execute_step(self) -> bool:
        """Update the Public Wiki.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing necessary clients and configuration.
        """
        self.__context.log("Updating public wiki...")

        if self.__context.dry_run:
            self.__context.log(f"Would update public wiki for DIMR version: {self.__context.dimr_version}")

        if self.__atlassian is None:
            self.__context.log("Atlassian client is required but not initialized")
            return False
        if self.__teamcity is None:
            self.__context.log("TeamCity client is required but not initialized")
            return False

        self.update_public_wiki()

        self.__context.log("Public wiki update completed successfully!")
        return True

    def update_public_wiki(self) -> None:
        """
        Create and/or update the Public Wiki for a specific DIMR version.

        Returns
        -------
        None
        """
        self.__context.log("Updating main wiki page...")
        if not self.__context.dry_run:
            main_page_id = self.__update_main_page()

        self.__context.log("Updating sub wiki page...")
        if not self.__context.dry_run:
            self.__update_sub_page(parent_page_id=main_page_id)

    def __update_main_page(self) -> str:
        """
        Update the content of the main page for the given DIMR version.

        Returns
        -------
        str
            The id of the updated page.
        """
        content = self.__prepare_content_for_main_page()
        page_to_update_page_id = self.__get_main_page_id_for_page_to_update()
        page_title = (
            f"{self.__dimr_patch_page_prefix} {self.__major_version}.{self.__minor_version}.{self.__patch_version}"
        )
        self.__update_content_of_page(page_to_update_page_id, page_title, content)

        return page_to_update_page_id

    def __prepare_content_for_main_page(self) -> str:
        """
        Prepare the content that should be uploaded to the main page.

        Returns
        -------
        str
            The content.
        """
        windows_version_artifact, linux_version_artifact = self.__get_version_artifacts()
        if windows_version_artifact is None:
            raise AssertionError("Could not retrieve the Windows version.txt artifact.")
        if linux_version_artifact is None:
            raise AssertionError("Could not retrieve the Linux version.txt artifact.")

        content = self.__create_content_from_template(
            windows_version_artifact=windows_version_artifact,
            linux_version_artifact=linux_version_artifact,
        )

        return content

    def __get_version_artifacts(self) -> Tuple[str, str]:
        """
        Get the latest Windows and Linux Version.txt artifacts from Dimr Collector Release.

        Returns
        -------
        Tuple[str, str]
            A tuple containing the Windows and Linux artifacts respectively.
        """
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")

        windows_collect_id = self.__teamcity.get_dependent_build_id(
            self.__build_id, self.__delft3d_windows_collect_build_type_id
        )

        windows_version_artifact = self.__teamcity.get_build_artifact(
            build_id=str(windows_collect_id) if windows_collect_id is not None else "",
            path_to_artifact=self.__path_to_windows_version_artifact,
        )
        linux_collect_id = self.__teamcity.get_dependent_build_id(
            self.__build_id, self.__delft3d_linux_collect_build_type_id
        )
        linux_version_artifact = self.__teamcity.get_build_artifact(
            build_id=str(linux_collect_id) if linux_collect_id is not None else "",
            path_to_artifact=self.__path_to_linux_version_artifact,
        )

        if windows_version_artifact is None or linux_version_artifact is None:
            raise ValueError("Could not retrieve version artifacts")

        return windows_version_artifact.decode(), linux_version_artifact.decode()

    def __create_content_from_template(self, windows_version_artifact: str, linux_version_artifact: str) -> str:
        """
        Create the content for the main DIMR page from a template file.

        Parameters
        ----------
        windows_version_artifact : str
            The Windows Version.txt artifact.
        linux_version_artifact : str
            The Linux Version.txt artifact.

        Returns
        -------
        str
            The content.
        """
        current_dir = os.path.dirname(__file__)
        path_to_wiki_template = os.path.join(current_dir, self.__relative_path_to_wiki_template)

        with open(path_to_wiki_template, "r") as file:
            data = file.read()

        data = data.replace("@@@DATE@@@", datetime.now(tz=timezone.utc).date().strftime("%d-%m-%Y"))
        data = data.replace("@@@WINDOWS_VERSION_ARTIFACT@@@", f"<pre>{windows_version_artifact}</pre>")
        data = data.replace("@@@LINUX_VERSION_ARTIFACT@@@", f"<pre>{linux_version_artifact}</pre>")
        data = data.replace(
            "@@@DIMR_RELEASE_VERSION@@@", f"{self.__major_version}.{self.__minor_version}.{self.__patch_version}"
        )
        return data

    def __get_main_page_id_for_page_to_update(self) -> str:
        """
        Get the page id for the Public Wiki page we want to update.

        This method first checks if there is a page for the current major version of DIMR on the root DIMR page.
        If there is no such page, it will be created. (e.g. DIMRset 2)

        It will then check if there is a page for the current major.minor version of DIMR under the major version page.
        If there is no such page, it will be created. (e.g. DIMRset 2 -> DIMRset 2.13)

        It will then check if there is a page for the current major.minor.patch version of DIMR under the major.minor
        version page.
        If there is no such page, it will be created. (e.g. DIMRset 2 -> DIMRset 2.13 -> DIMRset 2.13.03)

        It will then return the page id for the major.minor.patch page. This is the page that should be updated
        (so: the id of the DIMRset 2.13.03 page).

        Returns
        -------
        str
            The page id of the page to be updated.
        """
        dimr_major_version_page_id = self.__get_public_wiki_page_id(
            parent_page_id=self.__dimr_root_page_id,
            dimr_version=self.__major_version,
            prefix=self.__dimr_major_page_prefix,
        )

        dimr_minor_version_page_id = self.__get_public_wiki_page_id(
            parent_page_id=dimr_major_version_page_id,
            dimr_version=f"{self.__major_version}.{self.__minor_version}",
            prefix=self.__dimr_minor_page_prefix,
        )

        dimr_patch_version_page_id = self.__get_public_wiki_page_id(
            parent_page_id=dimr_minor_version_page_id,
            dimr_version=f"{self.__major_version}.{self.__minor_version}.{self.__patch_version}",
            prefix=self.__dimr_minor_page_prefix,
        )

        return dimr_patch_version_page_id

    def __get_public_wiki_page_id(self, parent_page_id: str, dimr_version: str, prefix: str, suffix: str = "") -> str:
        """
        Check if there is already a page for the specified DIMR version under the specified parent page.

        If there is no such page, it will be created.

        Returns
        -------
        str
            The id for the page of the specified DIMR version.
        """
        if self.__atlassian is None:
            raise ValueError("Atlassian client is required but not initialized")

        parent_page = self.__atlassian.get_page_info_for_parent_page(parent_page_id=parent_page_id)
        page_exists = False
        page_id = ""

        if parent_page is None:
            raise ValueError(f"Could not retrieve parent page info for page ID: {parent_page_id}")

        for result in parent_page["results"]:
            if "title" in result and result["title"] == f"{prefix} {dimr_version}{suffix}":
                page_exists = True
                page_id = result["id"]
                break

        if not page_exists:
            page_title = f"{prefix} {dimr_version}{suffix}"
            created_page_id = self.__atlassian.create_public_wiki_page(
                page_title=page_title, space_id=self.__dimr_space_id, ancestor_id=parent_page_id
            )
            if created_page_id is None:
                raise ValueError(f"Failed to create page: {page_title}")
            page_id = created_page_id

        if page_id == "" or page_id is None:
            raise AssertionError(f"Could not find or create the page for {prefix} {dimr_version}{suffix}.")

        return page_id

    def __update_content_of_page(self, page_id: str, page_title: str, content: str) -> None:
        """
        Update the page with the given page id on the Public Wiki with the given title and content.

        Parameters
        ----------
        page_id : str
            The id for the page to update.
        page_title : str
            The title for the page to update.
        content : str
            The content for the page to update.

        Returns
        -------
        None
        """
        if self.__atlassian is None:
            raise ValueError("Atlassian client is required but not initialized")

        page_updated_successfully = self.__atlassian.update_page(
            page_id=page_id, page_title=page_title, content=content
        )
        if not page_updated_successfully:
            raise AssertionError("Failed to update the public wiki page.")

    def __update_sub_page(self, parent_page_id: str) -> None:
        """
        Update the sub page for the given DIMR version.

        Parameters
        ----------
        parent_page_id : str
            The id for the main page of the specified DIMR version.

        Returns
        -------
        None
        """
        content = self.__prepare_content_for_sub_page()

        subpage_to_update_page_id = self.__get_public_wiki_page_id(
            parent_page_id=parent_page_id,
            dimr_version=self.__dimr_version,
            prefix=self.__dimr_subpage_prefix,
            suffix=self.__dimr_subpage_suffix,
        )
        page_title = f"{self.__dimr_subpage_prefix} {self.__dimr_version}{self.__dimr_subpage_suffix}"
        self.__update_content_of_page(page_id=subpage_to_update_page_id, page_title=page_title, content=content)

    def __prepare_content_for_sub_page(self) -> str:
        """
        Prepare the content that should be uploaded to the sub page.

        Returns
        -------
        str
            The content.
        """
        with open(self.__path_to_release_test_results_artifact, "rb") as f:
            artifact = f.read()

        # Add the <pre> ... </pre> tags to make sure the wiki page properly keeps the formatting
        content = f"<pre>{artifact.decode('utf-8')}</pre>"

        return content


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_git=False, require_ssh=False)
        services = Services(context)

        context.log("Starting public wiki update...")
        if WikiPublisher(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed public wiki update!")
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nPublic wiki update interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Public wiki update failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
