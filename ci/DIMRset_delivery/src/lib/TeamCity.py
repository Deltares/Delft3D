from typing import Any, Dict

import requests


class TeamCity(object):
    """
    Wrapper for the TeamCity REST API.

    TeamCity uses build type ids (Build configuration IDs) to identify builds.
    The build type id can be found in the Configuration Settings of a build.

    Each build type id has a list of builds. These are the builds that run
    after each commit or on a specific schedule, for example.

    Each build has a build number and a unique build id. The build number
    can be found in the list of builds in the GUI. The build id can be
    found in the URL when you click a specific build number.

    For more information about the TeamCity REST API, please see the official
    TeamCity REST API docs at:
    https://www.jetbrains.com/help/teamcity/rest-api.html
    """

    def __init__(self, username: str, password: str):
        """
        Instantiates a new TeamCity object.

        Args:
            username (str): Your Deltares username.
            password (str): Your Deltares password.
        """
        self.__auth = (username, password)
        self.__base_uri = "https://dpcbuild.deltares.nl/"
        self.__rest_uri = f"{self.__base_uri}app/rest/"
        self.__default_headers = {
            "content-type": "application/json",
            "accept": "application/json",
        }

    def test_api_connection(self) -> bool:
        """
        Tests if the the API connection can successfully
        be established.

        Uses the following TeamCity REST API endpoint:
        /app/rest/agents

        Returns:
            bool: Returns True if a successful request can be made.
        """
        endpoint = f"{self.__rest_uri}agents"
        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return True
        print("Could not connect to the TeamCity API:")
        print(f"Error: {result.status_code} - {result.content}")
        return False

    def get_project(self, project_id: str) -> Dict[str, str]:
        """
        Gets a teamcity project by its id

        Uses the following TeamCity REST API endpoint:
        /app/rest/projects?locator=id:<project_id>

        Args:
            project_id(str): the project id

        Returns:
            Dict[str, str]
        """
        endpoint = f"{self.__rest_uri}projects?locator=id:{project_id}"
        result = requests.get(
            endpoint, headers=self.__default_headers, auth=self.__auth
        )
        return result.json()

    def set_project_name(self, project_id: str, new_name: str) -> bytes:
        """
        Sets a new project name for a given project id

        Uses the following TeamCity REST API endpoint:
        /app/rest/projects/<project_id>/name

        Args:
            project_id(str): The id of the project to set the name on
            new_name(str): The new project name

        Returns:
            bytes: the new name as a byte string
        """
        endpoint = f"{self.__rest_uri}projects/{project_id}/name"
        result = requests.put(
            endpoint,
            headers=self.__get_put_request_headers(
                content_type="text/plain", accept="text/plain"
            ),
            auth=self.__auth,
            data=new_name,
            timeout=5,
        )
        return result.content

    def get_vcs_root_checkout_rules(self, build_id: str, vcs_root_name: str) -> bytes:
        """
        Reads the checkout rules of a version control root belonging to a build

        Uses the following TeamCity REST API endpoint:
        /app/rest/buildTypes/<build_id>/vcs-root-entries/<vcs_root_name>/checkout-rules

        Args:
            build_id(str): The build id with the version control root attached
            vcs_root_name(str): the version control root

        Returns:
            bytes: The checkout rules as a byte string
        """
        endpoint = f"{self.__rest_uri}buildTypes/{build_id}/vcs-root-entries/{vcs_root_name}/checkout-rules"
        result = requests.get(
            endpoint,
            headers={
                "content-type": "text/plain",
                "accept": "*/*",
                "origin": f"{self.__base_uri}",
            },
            auth=self.__auth,
        )

        return result.content

    def set_vcs_root_checkout_rules(
        self, build_id: str, vcs_root_name: str, checkout_rules: str
    ) -> bytes:
        """
        Sets the checkout rules of a version control root belonging to a build

        Uses the following TeamCity REST API endpoint:
        /app/rest/buildTypes/<build_id>/vcs-root-entries/<vcs_root_name>/checkout-rules

        Args:
            build_id(str): The build id with the version control root attached
            vcs_root_name(str): The version control root
            checkout_rules(str): The new checkout rule

        Returns:
            bytes: The new checkout rules as a byte string
        """
        endpoint = f"{self.__rest_uri}buildTypes/{build_id}/vcs-root-entries/{vcs_root_name}/checkout-rules"
        result = requests.put(
            endpoint,
            headers=self.__get_put_request_headers(
                content_type="text/plain", accept="text/plain"
            ),
            auth=self.__auth,
            data=checkout_rules,
        )

        return result.content

    def get_artifact_dependencies(self, build_type_id: str) -> bytes:
        """
        Gets the artifact dependencies belonging to a build_type_id

        Uses the following TeamCity REST API endpoint:
        /app/rest/buildTypes/<build_type_id>/artifact-dependencies

        Args:
            build_type_id(str): The id of the build with dependencies

        Returns:
            bytes: A byte string containing a dictionairy
        
        """ 
        endpoint = f"{self.__rest_uri}buildTypes/{build_type_id}/artifact-dependencies"

        result = requests.get(
            endpoint,
            headers=self.__default_headers,
            auth=self.__auth,
        )

        return result.content

    def toggle_artifact_dependency(
        self, build_type_id: str, artifact_id: str, enabled: bool
    ) -> bytes:
        """
        Enables/disables an artifact dependency of a build

        Uses the following TeamCity REST API endpoint:
        /app/rest/buildTypes/<build_type_id>/artifact-dependencies/<artifact_id>/disabled

        Args:
            build_type_id(str): The id of the build with the dependency to be dis- or enabled
            artifact_id(str): The id of the dependency to enable/disable
            enabled(bool): Determines whether or not the dependency should be enabled or disabled

        Returns:
            bytes: whether or not the dependency has been **disabled**. If you enable a dependency,
            the return will be False.
        """
        endpoint = f"{self.__rest_uri}buildTypes/{build_type_id}/artifact-dependencies/{artifact_id}/disabled"

        result = requests.put(
            endpoint,
            headers=self.__get_put_request_headers(
                content_type="text/plain", accept="text/plain"
            ),
            auth=self.__auth,
            data=str(not enabled),
        )
        return result.content

    def get_builds_for_build_type_id(
        self,
        build_type_id: str,
        limit: int = 10,
        include_failed_builds: bool = False,
        pinned: str = "any",
    ) -> Dict[str, Any]:
        """
        Gets builds for the given build type id.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds?<buildLocator>

        Args:
            build_type_id (str): The build type id.
            limit (int, optional): The maximum number of builds you want to get.
                Defaults to 10.
            include_failed_builds (bool, optional): Specifies whether to include
                builds that have failed. Defaults to False.
            pinned (str, optional): Specifies whether to get only pinned build, builds that are not pinned, or both.
                Possible values are: "true", "false" or "any". Defaults to "any".

        Returns:
            Dict[str, Any]: A dictionary with a variety of keys. The 'build' key
            contains a list of builds for the given build type id.

            For more information, please see the official TeamCity REST API docs.

            Returns None if the request failed.
        """

        status_locator = ""
        if include_failed_builds is True:
            status_locator = ",status:SUCCESS"

        pinned_locator = ""
        if pinned == "true" or pinned == "false":
            pinned_locator = f",pinned:{pinned}"

        endpoint = f"{self.__rest_uri}builds?locator=defaultFilter:false,buildType:{build_type_id},count:{limit}{status_locator}{pinned_locator}"

        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return result.json()
        print(f"Could not retrieve builds for build id {build_type_id}:")
        print(f"{result.status_code} - {result.content}")
        return None

    def get_latest_build_for_build_type_id(
        self, build_type_id: str, include_failed_builds: bool = False
    ) -> Dict[str, Any]:
        """
        Gets the latest build for the given build type id.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds?<buildLocator>

        Args:
            build_type_id (str): The build type id.
            include_failed_builds (bool, optional): Specifies whether to include
                builds that have failed. Defaults to False.

        Returns:
            Dict[str, Any]: A dictionary with a variety of keys. The 'build' key
            contains a list of builds for the given build type id.

            For more information, please see the official TeamCity REST API docs.

            Returns None if the request failed.
        """
        return self.get_builds_for_build_type_id(
            build_type_id=build_type_id,
            limit=1,
            include_failed_builds=include_failed_builds,
        )

    def get_latest_build_number_for_build_type_id(
        self, build_type_id: str, include_failed_builds=False
    ) -> str:
        """
        Gets the latest build number for a given build type id.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds?<buildLocator>

        Args:
            build_type_id (str): The build type id.
            include_failed_builds (bool, optional): Specifies whether to include
                builds that have failed. Defaults to False.

        Returns:
            str: The build number of the latest build.

            Returns None if the request failed.
        """
        latest_build = self.get_latest_build_for_build_type_id(
            build_type_id=build_type_id, include_failed_builds=include_failed_builds
        )
        if latest_build is None:
            return None
        return latest_build["build"][0]["number"]

    def get_build_id_for_specific_build(
        self, build_type_id: str, build_number: str
    ) -> str:
        """
        Gets the unique build id for a specific build.

        Uses the following TeamCity REST API endpoint:
        /app/rest/buildTypes/<buildTypeLocator>/builds?<buildLocator>

        Args:
            build_type_id (str): The build type id.
            build_number (str): The build number of a specific build.

        Returns:
            str: The build id for a specific build.

            Returns None if the request has failed.

        """
        endpoint = f"{self.__rest_uri}buildTypes/id:{build_type_id}/builds?locator=defaultFilter:false,number:{build_number}"
        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return result.json()["build"][0]["id"]
        print(
            f"Could not retrieve build id for build type {build_type_id} and build number {build_number}:"
        )
        print(f"{result.status_code} - {result.content}")
        return None

    def get_build_info_for_build_id(self, build_id: str) -> Dict[str, Any]:
        """
        Gets the build info for a specific build.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds/<buildLocator>

        Arguments:
            build_id (str): The build id of a specific build.

        Returns:
            Dict[str, Any]: A dictionary with a variety of keys. This includes
            information about the configuration parameters, artifact dependencies
            and the agent the build has run on.

            For more information, please see the official TeamCity REST API docs.

            Returns None if the request failed.
        """
        endpoint = f"{self.__rest_uri}builds/id:{build_id}?fields=id,artifact-dependencies(build),resultingProperties(property)"
        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return result.json()
        print(f"Could not retrieve build info for build id {build_id}:")
        print(f"{result.status_code} - {result.content}")
        return None

    def get_build_info_for_latest_build_for_build_type_id(
        self, build_type_id: str, include_failed_builds: bool = False
    ) -> Dict[str, Any]:
        """
        Gets the build info for the latest build of a specific build type.

        Uses the following TeamCity REST API endpoints:
        /app/rest/builds?<buildLocator>
        /app/rest/buildTypes/<buildTypeLocator>/builds?<buildLocator>

        Arguments:
            build_type_id (str): The build type id.
            include_failed_builds (bool, optional): Specifies whether to include
                builds that have failed. Defaults to False.

        Returns:
            Dict[str, Any]: A dictionary with a variety of keys. This includes
            information about the configuration parameters, artifact dependencies
            and the agent the build has run on.

            For more information, please see the official TeamCity REST API docs.

            Returns None if the request failed.
        """
        latest_build_id = self.get_latest_build_id_for_build_type_id(
            build_type_id=build_type_id, include_failed_builds=include_failed_builds
        )
        if latest_build_id is None:
            return None
        return self.get_build_info_for_build_id(build_id=latest_build_id)

    def get_latest_build_id_for_build_type_id(
        self, build_type_id: str, include_failed_builds: bool = False
    ) -> str:
        """
        Gets the build id for the latest build of a specific build type.

        Uses the following TeamCity REST API endpoints:
        /app/rest/builds?<buildLocator>
        /app/rest/buildTypes/<buildTypeLocator>/builds?<buildLocator>

        Arguments:
            build_type_id (str): The build type id.
            include_failed_builds (bool, optional): Specifies whether to include
                builds that have failed. Defaults to False.

        Returns:
            str: The build id for the latest build for a specific build type.

            Returns None if the request failed.
        """
        latest_build_number = self.get_latest_build_number_for_build_type_id(
            build_type_id=build_type_id, include_failed_builds=include_failed_builds
        )
        if latest_build_number is None:
            return None
        latest_build_id = self.get_build_id_for_specific_build(
            build_type_id=build_type_id, build_number=latest_build_number
        )
        return latest_build_id

    def get_build_artifact_names(self, build_id: str) -> Dict[str, Any]:
        """
        Gets the names of the artifact files and folders.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds/<buildLocator>/artifacts/children

        Arguments:
            build_id (str): The build id for a specific build.

        Dict[str, Any]: A dictionary with a variety of keys. This includes
            a 'count' key specifying the number of files/folders, and a
            'files' key. The 'files' key is another dictionary that contains
            several more keys, including a 'name' key that specifies the name
            of the file/folder.

            For more information, please see the official TeamCity REST API docs.

            Returns None if the request failed.
        """
        endpoint = f"{self.__rest_uri}builds/buildId:{build_id}/artifacts/children"
        headers = self.__get_put_request_headers()
        result = requests.get(url=endpoint, headers=headers, auth=self.__auth)
        if result.status_code == 200:
            return result.json()
        print(f"Could not get artifact names for build id {build_id}:")
        print(f"{result.status_code} - {result.content}")
        return None

    def get_build_artifact(self, build_id: str, path_to_artifact: str) -> bytes:
        """
        Gets a specific artifact for a specific build.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds/<buildLocator>/artifacts/contents/<path>

        Arguments:
            build_id (str): The build id for a specific build.
            path_to_artifact (str): The path to the artifact. The path
            can span into the archive content, for example:
            dir/path/archive.zip!/path_within_archive

        Returns:
            bytes: Returns the file.

            Returns None if the request failed or the specified file
            could not be found.

        """
        endpoint = f"{self.__rest_uri}builds/buildId:{build_id}/artifacts/content/{path_to_artifact}"
        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return result.content
        print(
            f"Could not get artifact for build id {build_id} and path {path_to_artifact}:"
        )
        print(f"{result.status_code} - {result.content}")
        return None

    def pin_build(self, build_id: str) -> bool:
        """
        Pins a specific build.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds/<buildLocator>/pin

        Arguments:
            build_id (str): The build id for a specific build.

        Returns:
            bool: Returns True if the build was successfully pinned.
        """
        endpoint = f"{self.__rest_uri}builds/buildId:{build_id}/pin"
        headers = self.__get_put_request_headers(content_type="text/plain")
        result = requests.put(url=endpoint, headers=headers, auth=self.__auth)
        if result:
            return True
        print(f"Could not pin build with build id {build_id}:")
        print(f"{result.status_code} - {result.content}")
        return False

    def add_tag_to_build(self, build_id: str, tag: str) -> bool:
        """
        Adds the specified tag to the specified build.

        Uses the following TeamCity REST API endpoint:
        /app/rest/builds/<buildLocator>/tag

        Arguments:
            build_id (str): The build id for a specific build.
            tag (str): The tag to add.

        Returns:
            bool: Returns True if the tags were successfully
            added to the specified build.
        """
        endpoint = f"{self.__rest_uri}builds/buildId:{build_id}/tags"
        headers = {
            "content-type": "text/plain",
            "accept": "*/*",
            "origin": f"{self.__base_uri}",
        }
        payload = tag
        result = requests.post(
            url=endpoint, headers=headers, auth=self.__auth, data=payload
        )
        if result.status_code == 200:
            return True
        print(f"Could not add {tag} tag to build with build id {build_id}:")
        print(f"{result.status_code} - {result.content}")
        return False

    def __get_csrf_token(self) -> str:
        """
        Gets a CSRF token required for making requests other
        than GET requests.

        Does not use the TeamCity REST API, but makes a GET
        request to the following URI to retrieve the token for
        the session.

        Returns:
            str: The CSRF token.

            Returns None if the request failed.
        """
        endpoint = f"{self.__base_uri}authenticationTest.html?csrf"
        result = requests.get(
            url=endpoint, headers=self.__default_headers, auth=self.__auth
        )
        if result.status_code == 200:
            return result.content
        return None

    def __get_put_request_headers(
        self, content_type: str = "application/json", accept: str = "application/json"
    ) -> Dict[str, str]:
        """
        Creates the headers required for making PUT requests.

        Returns:
            Dict[str, str]: The headers required for making PUT
            requests.
        """
        headers = {
            "content-type": content_type,
            "accept": accept,
            "X-TC-CSRF-Token": self.__get_csrf_token(),
            "origin": self.__base_uri,
        }
        return headers
