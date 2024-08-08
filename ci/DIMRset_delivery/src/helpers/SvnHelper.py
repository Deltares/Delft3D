import os
import re
import shutil
import subprocess
import tempfile
from typing import Dict
from datetime import date

from lib.TeamCity import TeamCity
from settings.teamcity_settings import TOOLS, DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID, \
    PATH_TO_WINDOWS_VERSION_ARTIFACT


class SvnHelper(object):
    """ Class responsible for executing SVN commands. """

    def __init__(self, teamcity: TeamCity, svn_revision_number: str, dimr_version: str):
        """
        Creates a new instance of SvnHelper.

        Arguments:
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            svn_revision_number (str): The SVN revision number of the DIMR build.
        """
        self.__teamcity = teamcity
        self.__svn_revision_number = svn_revision_number
        self.__dimr_version = dimr_version

    def tag_in_svn(self, svn_url: str) -> None:
        """ Tags the build in SVN, based on Edwin Spee's script. """
        self.OSS_svn_number = self.__get_OSS_svn_number()

        svn_tags = f"https://svn.oss.deltares.nl/repos/delft3d/tags/delft3dfm/{self.OSS_svn_number}"
        
        svn_temporary_log_message_filename = self.__create_temporary_svn_log_message_file()

        subprocess.call(['svn', 'copy', '-r', self.OSS_svn_number, '-F', svn_temporary_log_message_filename,
                         svn_url, svn_tags], shell=False)
        #
        # If the error "File not found" appears:
        # Check that svn.exe is installed and available via PATH
        # By default, Tortoise does not install svn. During installation of Tortoise it's optional to also install svn

    def __create_temporary_svn_log_message_file(self) -> str:
        """ Creates a temporary file that contains the SVN log message. """
        tools = self.__get_tool_versions_for_svn_log()

        log = f"From OSS trunk revision {self.OSS_svn_number}:\n"
        for TOOL in TOOLS:
            name = TOOL.name_for_svn_log
            version = tools[TOOL.name_for_extracting_version]
            log += "{0:<10}".format(name) + f"Version {version}\n"
        curdat = date.today().strftime("%d %B %Y")
        log += f"Included in DIMRset {self.__dimr_version}.{self.__svn_revision_number}, {curdat}"


        file_handle, temporary_svn_log_file_path = tempfile.mkstemp()
        os.write(file_handle, log.encode())
        os.close(file_handle)
        shutil.copy(temporary_svn_log_file_path, 'svn.txt')
        return temporary_svn_log_file_path

    def __get_tool_versions_for_svn_log(self) -> Dict[str, str]:
        """ Extracts the version numbers for the tools from the correct build artifact. """
        tools = {}
        for TOOL in TOOLS:
            tools[TOOL.name_for_extracting_version] = None

        build_to_get_artifact_from = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID)
        artifact_to_extract_version_from = self.__teamcity.get_build_artifact(
            build_id=build_to_get_artifact_from, path_to_artifact=PATH_TO_WINDOWS_VERSION_ARTIFACT)

        artifact_text = artifact_to_extract_version_from.decode()

        for TOOL in TOOLS:
            version = re.findall(f"{TOOL.name_for_extracting_version} Version ([0-9 .]*)", artifact_text)[0]
            tools[TOOL.name_for_extracting_version] = version
        self.__assert_all_tool_versions_have_been_extracted(tools)
        return tools

    def __assert_all_tool_versions_have_been_extracted(self, tools: Dict[str, str]):
        """ Asserts all version numbers for the tools have been found. """
        missing_tool_versions = []
        for TOOL in TOOLS:
            name = TOOL.name_for_extracting_version
            if tools[name] is None:
                missing_tool_versions.append(name)
            else:
                print(f"Found {name} Version {tools[name]}.")

        if len(missing_tool_versions) == 0:
            return

        error = "Could not find the version number for the following tools: \n"
        error += ', '.join(missing_tool_versions)
        raise AssertionError(error)

    def __get_OSS_svn_number(self) -> str:
        """ Gets the correct svn number that should be used "when tagging in SVN. """
        tools = self.__get_tool_versions_for_svn_log()
        for TOOL in TOOLS:
            if TOOL.name_for_extracting_version == "DIMR_EXE":
                dimr_version = tools[TOOL.name_for_extracting_version].split('.')
                return dimr_version[-1].strip()
        return ""
