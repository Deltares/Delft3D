import sys
from types import SimpleNamespace
from typing import Any, Dict, List, Optional, Union

import requests
from requests import Response

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface
from ci_tools.dimrset_delivery.settings.teamcity_settings import KERNELS
from ci_tools.dimrset_delivery.teamcity_types import ConfigurationTestResult
from ci_tools.example_utils.logger import LogLevel


class TeamCity(ConnectionServiceInterface):
    """
    Wrapper for the TeamCity REST API.

    Provides methods to interact with TeamCity builds, artifacts, tags, and test results.
    Usage: Instantiate with credentials and context, then call API methods.
    """

    def __init__(self, credentials: Credentials, context: DimrAutomationContext) -> None:
        """
        Instantiate a new TeamCity object.

        Parameters
        ----------
        credentials : Credentials
            Username and Password for authentication.
        context : DimrAutomationContext
            Automation context for logging and configuration.
        """
        self.__auth = (credentials.username, credentials.password)
        self.__base_uri = "https://dpcbuild.deltares.nl"
        self.__rest_uri = f"{self.__base_uri}/app/rest"
        self.__default_headers = {
            "content-type": "application/json",
            "accept": "application/json",
        }
        self.__context = context

    def test_connection(self) -> bool:
        """
        Test if the API connection can be established.

        Returns
        -------
        bool
            True if a successful request can be made.
        """
        self.__context.log(f"Checking connection to the TeamCity API with credentials: {self.__auth[0]}")
        endpoint = f"{self.__rest_uri}/agents"
        if self.__context.dry_run:
            self.__context.log(f"GET request: {endpoint}")
            result: Union[Response, SimpleNamespace] = SimpleNamespace(status_code=200, content=b"dry-run mock")
        else:
            result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            self.__context.log("Successfully connected to the TeamCity API.")
            success = True
        else:
            self.__context.log("Could not connect to the TeamCity API:", severity=LogLevel.ERROR)
            self.__context.log(
                f"Error: {result.status_code} - {result.content.decode('utf-8', errors='replace')}",
                severity=LogLevel.ERROR,
            )
            success = False

        return success

    def get_builds_for_build_configuration_id(
        self,
        build_configuration_id: str,
        limit: int = 10,
        include_failed_builds: bool = False,
        pinned: str = "any",
    ) -> Optional[Dict[str, Any]]:
        """
        Get builds for the given build configuration ID.

        Parameters
        ----------
        build_configuration_id : str
            Build configuration ID.
        limit : int, optional
            Maximum number of builds to retrieve. Defaults to 10.
        include_failed_builds : bool, optional
            Whether to include failed builds. Defaults to False.
        pinned : str, optional
            Filter for pinned builds ("true", "false", "any"). Defaults to "any".

        Returns
        -------
        Optional[Dict[str, Any]]
            Dictionary containing build information, or None if request failed.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        pinned_locator = ""
        if pinned == "true" or pinned == "false":
            pinned_locator = f",pinned:{pinned}"

        status_locator = ""
        if not include_failed_builds:
            status_locator = ",status:SUCCESS"

        endpoint = (
            f"{self.__rest_uri}/builds?locator=defaultFilter:false,"
            f"buildType:{build_configuration_id},count:{limit}{pinned_locator}{status_locator}"
        )

        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            json_result: Dict[str, Any] = result.json()
            return json_result
        self.__context.log(f"Could not retrieve builds for build id: {build_configuration_id}", severity=LogLevel.ERROR)
        self.__context.log(
            f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
        )
        sys.exit(result.status_code)

    def get_build_info_for_build_id(self, build_id: str) -> Optional[Dict[str, Any]]:
        """
        Get build info for a specific build.

        Parameters
        ----------
        build_id : str
            Build ID.

        Returns
        -------
        Optional[Dict[str, Any]]
            Dictionary with build information, or None if request failed.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        endpoint = (
            f"{self.__rest_uri}/builds/id:{build_id}?"
            f"fields=id,artifact-dependencies(build),resultingProperties(property)"
        )
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            build_info: Dict[str, Any] = result.json()
            return build_info
        self.__context.log(f"Could not retrieve build info for build id: {build_id}", severity=LogLevel.ERROR)
        self.__context.log(
            f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
        )
        sys.exit(result.status_code)

    def get_full_build_info_for_build_id(self, build_id: str) -> Optional[Dict[str, Any]]:
        """
        Get full build info for a specific build.

        Parameters
        ----------
        build_id : str
            Build ID.

        Returns
        -------
        Optional[Dict[str, Any]]
            Dictionary with full build information, or None if request failed.
        """
        endpoint = f"{self.__rest_uri}/builds/id:{build_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            full_build_info: Dict[str, Any] = result.json()
            return full_build_info
        self.__context.log(f"Could not retrieve build info for build id: {build_id}")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}")
        return None

    def get_build_artifact_names(self, build_id: str) -> Optional[Dict[str, Any]]:
        """
        Get names of artifact files and folders for a build.

        Parameters
        ----------
        build_id : str
            Build ID.

        Returns
        -------
        Optional[Dict[str, Any]]
            Dictionary with artifact names, or None if request failed.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        endpoint = f"{self.__rest_uri}/builds/buildId:{build_id}/artifacts/children"
        headers = self.__get_put_request_headers()
        result = requests.get(url=endpoint, headers=headers, auth=self.__auth)
        if result.status_code == 200:
            artifact_names: Dict[str, Any] = result.json()
            return artifact_names
        self.__context.log(f"Could not get artifact names for build id: {build_id}", severity=LogLevel.ERROR)
        self.__context.log(
            f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
        )
        sys.exit(result.status_code)

    def get_build_artifact(self, build_id: str, path_to_artifact: str) -> Optional[bytes]:
        """
        Get a specific artifact for a build.

        Parameters
        ----------
        build_id : str
            Build ID.
        path_to_artifact : str
            Path to the artifact, including archive paths if needed.

        Returns
        -------
        Optional[bytes]
            File content, or None if request failed.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        endpoint = f"{self.__rest_uri}/builds/buildId:{build_id}/artifacts/content/{path_to_artifact}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            return result.content
        self.__context.log(
            f"Could not get artifact for build id {build_id} and path '{path_to_artifact}'", severity=LogLevel.ERROR
        )
        self.__context.log(
            f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
        )
        sys.exit(result.status_code)

    def pin_build(self, build_id: str) -> bool:
        """
        Pin a specific build.

        Parameters
        ----------
        build_id : str
            Build ID.

        Returns
        -------
        bool
            True if the build was successfully pinned, False otherwise.
        """
        endpoint = f"{self.__rest_uri}/builds/buildId:{build_id}/pin"
        headers = self.__get_put_request_headers(content_type="text/plain")
        result = requests.put(url=endpoint, headers=headers, auth=self.__auth)
        if result:
            return True
        self.__context.log(f"Could not pin build with build id {build_id}:")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}")
        return False

    def add_tag_to_build_with_dependencies(self, build_id: str, tag: str) -> bool:
        """
        Add a tag to a build and its dependencies.

        Parameters
        ----------
        build_id : str
            Build ID.
        tag : str
            Tag to add.

        Returns
        -------
        bool
            True if tags were successfully added.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        endpoint = f"{self.__rest_uri}/builds/buildId:{build_id}/tags"
        headers = {
            "content-type": "text/plain",
            "accept": "*/*",
            "origin": f"{self.__base_uri}",
        }
        payload = tag
        result = requests.post(url=endpoint, headers=headers, auth=self.__auth, data=payload)
        if result.status_code != 200:
            self.__context.log(f"Could not add {tag} tag to build with build id: {build_id}", severity=LogLevel.ERROR)
            self.__context.log(
                f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
            )
            sys.exit(result.status_code)

        endpoint = f"{self.__rest_uri}/builds/multiple/snapshotDependency:(to:(id:{build_id})),count:1000/tags"
        csrf_token = self.__get_csrf_token()
        headers = {
            "Content-Type": "application/json",
            "X-TC-CSRF-Token": csrf_token or "",
        }
        payload = f'{{"tag":[{{"name":"{tag}"}}]}}'
        result = requests.post(url=endpoint, headers=headers, auth=self.__auth, data=payload)
        if result.status_code != 200:
            self.__context.log(
                f"Could not add {tag} tag to dependencies of build with build id: {build_id}", severity=LogLevel.ERROR
            )
            self.__context.log(
                f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
            )
            sys.exit(result.status_code)
        return True

    def get_dependent_build_ids_with_filter(self, build_id: str, filtered_ids: List[str]) -> List[str]:
        """
        Get build IDs for builds related to a specific build, filtered by buildTypeIds.

        Parameters
        ----------
        build_id : str
            Build ID to filter related builds.
        filtered_ids : List[str]
            List of TeamCity build type IDs to filter by.

        Returns
        -------
        List[str]
            List of build IDs matching the filter.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        if self.__context.dry_run:
            self.__context.log(f"Would get dependency chain for build {self.__context.build_id}")
            if filtered_ids:
                self.__context.log(f"Would filter by build types: {filtered_ids}")
            return ["123456", "123457", "123458"]

        build_type_ids = filtered_ids
        endpoint = (
            f"{self.__rest_uri}/builds?locator=defaultFilter:false,"
            f"snapshotDependency(to:(id:{build_id})),count:1000&fields=build(id,buildTypeId)"
        )
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            build_data = result.json()
            build_ids = []
            for build in build_data.get("build", []):
                dependency_build_id = str(build.get("id"))
                build_type_id = build.get("buildTypeId")
                if dependency_build_id and build_type_id and build_type_id in build_type_ids:
                    build_ids.append(dependency_build_id)
            return build_ids
        self.__context.log(f"Could not retrieve build info for build id {build_id}:", severity=LogLevel.ERROR)
        self.__context.log(
            f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}", severity=LogLevel.ERROR
        )
        sys.exit(result.status_code)

    def get_dependent_build_id(self, build_id: str, dependent_build_type: str) -> Optional[int]:
        """
        Get the build ID of a specific dependent build type for a given build.

        Parameters
        ----------
        build_id : str
            Build ID.
        dependent_build_type : str
            Build type ID of the dependent build.

        Returns
        -------
        Optional[int]
            Build ID of the dependent build, or None if not found.
        """
        endpoint = (
            f"{self.__rest_uri}/builds?locator=defaultFilter:false,"
            f"snapshotDependency(to:(id:{build_id})),count:1000&fields=build(id,buildTypeId)"
        )
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            build_data = result.json()
            for build in build_data.get("build", []):
                dependent_build_type_id = str(build.get("buildTypeId"))
                if dependent_build_type_id == dependent_build_type:
                    build_id_value: Optional[int] = build.get("id")
                    return build_id_value

        self.__context.log(f"Could not retrieve dependent build ({dependent_build_type}) for build id {build_id}:")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8', errors='replace')}")
        return None

    def get_build_test_results_from_teamcity(self, build_id: str) -> Optional[ConfigurationTestResult]:
        """
        Fetch test results for a given build ID.

        Parameters
        ----------
        build_id : str
            Build ID to retrieve results for.

        Returns
        -------
        Optional[ConfigurationTestResult]
            Parsed test results for the build, or None if no test results.
        """
        if self.__context.dry_run:
            self.__context.log(f"Would get test results for build {build_id}")
            return ConfigurationTestResult(
                name=f"{self.__context.settings.dry_run_prefix} Test Configuration / Build {build_id}",
                build_nr=str(build_id),
                passed=85,
                failed=0,
                ignored=0,
                muted=0,
                status_text=f"{self.__context.settings.dry_run_prefix} SUCCESS",
            )

        build_info = self.get_full_build_info_for_build_id(build_id)
        if not build_info:
            return None

        test_occurrences = build_info.get("testOccurrences", {})
        if not test_occurrences or int(test_occurrences.get("count", "0")) == 0:
            return None

        build_nr = build_info.get("number", "Unknown build number")
        status_text = build_info.get("status", "No status available")

        config_name = "Unknown config"
        parent = "Unknown parent"
        build_type = build_info.get("buildType", {})
        if build_type:
            config_name = build_type.get("name", "Unknown config")
            parent = build_type.get("projectName", "Unknown parent")
        config_name = f"{parent} / {config_name}"

        passed = int(test_occurrences.get("passed", "0"))
        failed = int(test_occurrences.get("failed", "0"))
        ignored = int(test_occurrences.get("ignored", "0"))
        muted = int(test_occurrences.get("muted", "0"))

        return ConfigurationTestResult(
            name=config_name,
            build_nr=build_nr,
            passed=passed,
            failed=failed,
            ignored=ignored,
            muted=muted,
            status_text=status_text,
        )

    def get_kernel_versions(self) -> Dict[str, str]:
        """
        Get kernel versions from context.

        Returns
        -------
        Dict[str, str]
            Dictionary mapping kernel names to their versions.

        Raises
        ------
        ValueError
            If build info cannot be retrieved.
        """
        if self.__context.dry_run:
            self.__context.log(
                f"Get build info of build_id {self.__context.build_id}, then extract kernel versions from properties."
            )
            kernel_versions = {
                KERNELS[0].name_for_extracting_revision: "1.23.45",
                KERNELS[1].name_for_extracting_revision: "abcdefghijklmnopqrstuvwxyz01234567890123",
            }
        else:
            publish_build_info = self.get_build_info_for_build_id(self.__context.build_id)
            if publish_build_info is None:
                raise ValueError("Could not retrieve build info from TeamCity")
            kernel_versions = self.__extract_kernel_versions(publish_build_info)
        return kernel_versions

    def get_dimr_version(self) -> str:
        """
        Get DIMR version from context.

        Returns
        -------
        str
            DIMR version string.

        Raises
        ------
        AssertionError
            If kernel versions have not been extracted.
        """
        kernel_versions = self.get_kernel_versions()
        if kernel_versions is None:
            raise AssertionError("Could not extract the DIMR version: the kernel versions have not yet been extracted")
        dimr_version = kernel_versions["DIMRset_ver"]

        if dimr_version is None:
            raise AssertionError("DIMR version is unexpectedly None after extraction")

        return dimr_version

    def get_branch_name(self) -> str:
        """
        Get branch name from context.

        Returns
        -------
        str
            Branch name.

        Raises
        ------
        ValueError
            If branch name could not be retrieved.
        """
        branch_name = None
        if self.__context.dry_run:
            self.__context.log(
                f"Get build info of build_id {self.__context.build_id}, then get branch name from properties."
            )
            branch_name = "main"
            self.__context.log(f"simulating '{branch_name}' branch")
            return branch_name

        latest_publish_build_info = self.get_build_info_for_build_id(self.__context.build_id)
        if latest_publish_build_info is None:
            raise ValueError("Could not retrieve build info from TeamCity")
        branch_name_property = next(
            (
                prop
                for prop in latest_publish_build_info["resultingProperties"]["property"]
                if prop["name"] == "teamcity.build.branch"
            ),
            None,
        )
        if branch_name_property is None:
            raise ValueError("Could not find branch name in build properties")
        branch_name = branch_name_property["value"]

        if branch_name is None:
            raise ValueError("Branch name is unexpectedly None after retrieval")

        return branch_name

    def __get_put_request_headers(
        self, content_type: str = "application/json", accept: str = "application/json"
    ) -> Dict[str, str]:
        """
        Create headers for PUT requests.

        Parameters
        ----------
        content_type : str, optional
            Content type header. Defaults to "application/json".
        accept : str, optional
            Accept header. Defaults to "application/json".

        Returns
        -------
        Dict[str, str]
            Headers for PUT requests.
        """
        csrf_token = self.__get_csrf_token()
        headers = {
            "content-type": content_type,
            "accept": accept,
            "X-TC-CSRF-Token": csrf_token or "",
            "origin": self.__base_uri,
        }
        return headers

    def __get_csrf_token(self) -> Optional[str]:
        """
        Get CSRF token for non-GET requests.

        Returns
        -------
        Optional[str]
            CSRF token, or None if request failed.

        Raises
        ------
        SystemExit
            If the request fails.
        """
        endpoint = f"{self.__base_uri}/authenticationTest.html?csrf"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth)
        if result.status_code == 200:
            return result.content.decode("utf-8", errors="replace")
        sys.exit(result.status_code)

    def __extract_kernel_versions(self, build_info: Dict[str, Any]) -> Dict[str, str]:
        """
        Extract kernel versions from build info.

        Parameters
        ----------
        build_info : Dict[str, Any]
            Build information dictionary.

        Returns
        -------
        Dict[str, str]
            Dictionary mapping kernel names to their versions.
        """
        kernel_versions: Dict[str, str] = {}
        for kernel in KERNELS:
            kernel_versions[kernel.name_for_extracting_revision] = ""

        for kernel_prop in build_info["resultingProperties"]["property"]:
            if any(k.name_for_extracting_revision == kernel_prop["name"] for k in KERNELS):
                kernel_versions[kernel_prop["name"]] = kernel_prop["value"]

        return kernel_versions
