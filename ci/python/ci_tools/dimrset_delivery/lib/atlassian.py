import json
from types import SimpleNamespace
from typing import Any, Dict, Optional, Union

import requests

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface
from ci_tools.example_utils.logger import LogLevel


class Atlassian(ConnectionServiceInterface):
    """
    Atlassian Confluence REST API wrapper.

    Provides methods to interact with the Atlassian Confluence API, including page creation, update, and retrieval.
    Usage:
        client = Atlassian(username, password, context)
        client.create_public_wiki_page(...)
    """

    def __init__(self, credentials: Credentials, context: DimrAutomationContext) -> None:
        """
        Initialize Atlassian API client.

        Parameters
        ----------
        credentials : Credentials
            Username and Password for authentication.
        context : DimrAutomationContext
            Automation context for logging and configuration.
        """
        self.__auth = (credentials.username, credentials.password)
        self.__rest_uri = "https://publicwiki.deltares.nl/rest/api"
        self.__default_headers = {"content-type": "application/json", "accept": "application/json"}
        self.__context = context

    def test_connection(self) -> bool:
        """
        Test API connection to Atlassian Confluence.

        Returns
        -------
        bool
            True if connection is successful, False otherwise.
        """
        self.__context.log(f"Checking connection to the Atlassian Confluence API with credentials: {self.__auth[0]}")
        endpoint = f"{self.__rest_uri}/content"

        if self.__context.dry_run:
            self.__context.log(f"GET request: {endpoint}")
            result: Union[SimpleNamespace, requests.Response] = SimpleNamespace(
                status_code=200, content=b"dry-run mock"
            )
        else:
            result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)

        if result.status_code == 200:
            self.__context.log("Successfully connected to the Atlassian Confluence API.")
            success = True
        else:
            self.__context.log("Could not connect to the Atlassian Confluence API:")
            self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}")
            success = False

        return success

    def get_page_info_for_parent_page(self, parent_page_id: str) -> Optional[Dict[str, Any]]:
        """
        Get page information for a given parent page.

        Parameters
        ----------
        parent_page_id : str
            ID of the parent page.

        Returns
        -------
        Optional[Dict[str, Any]]
            Dictionary with page info if successful, None otherwise.
        """
        endpoint = f"{self.__rest_uri}/content/search?cql=parent={parent_page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            json_response: Dict[str, Any] = result.json()
            return json_response
        self.__context.log(
            (
                f"Could not get page info for page {parent_page_id}: "
                f"{result.status_code} - {result.content.decode('utf-8')}"
            )
        )
        return None

    def create_public_wiki_page(
        self, page_title: str, space_id: str, ancestor_id: str, body: str = ""
    ) -> Optional[str]:
        """
        Create a new page on the Public Wiki.

        Parameters
        ----------
        page_title : str
            Title for the new page.
        space_id : str
            Space key for the page.
        ancestor_id : str
            ID of the parent page.
        body : str, optional
            Content for the new page. Defaults to empty string.

        Returns
        -------
        Optional[str]
            ID of the newly created page if successful, None otherwise.
        """
        payload = json.dumps(
            {
                "title": page_title,
                "type": "page",
                "space": {"key": space_id},
                "status": "current",
                "ancestors": [{"id": ancestor_id}],
                "body": {"storage": {"value": str(body), "representation": "storage"}},
            }
        )

        endpoint = f"{self.__rest_uri}/content"
        result = requests.post(
            url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False
        )
        if result.status_code == 200:
            self.__context.log("Successfully created page.")
            json_response: Dict[str, Any] = result.json()
            page_id: str = json_response["id"]
            return page_id
        self.__context.log("Could not create page:")
        self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}", severity=LogLevel.ERROR)
        return None

    def update_page(self, page_id: str, page_title: str, content: str, next_version: Optional[int] = None) -> bool:
        """
        Update an existing page on the Public Wiki.

        Parameters
        ----------
        page_id : str
            ID of the page to update.
        page_title : str
            New title for the page.
        content : str
            New content for the page.
        next_version : Optional[int]
            Next version number. If None, will auto-increment.

        Returns
        -------
        bool
            True if the page was updated successfully, False otherwise.
        """
        if next_version is None:
            current_version = self.__get_page_version(page_id)
            if current_version is None:
                self.__context.log("Could not update page:")
                self.__context.log("Could not retrieve the current version of the page.")
                return False
            next_version = current_version + 1

        payload = json.dumps(
            {
                "version": {"number": next_version},
                "title": page_title,
                "type": "page",
                "status": "current",
                "body": {"storage": {"value": str(content), "representation": "storage"}},
            }
        )

        endpoint = f"{self.__rest_uri}/content/{page_id}"
        result = requests.put(
            url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False
        )
        if result.status_code != 200:
            self.__context.log("Could not update page:")
            self.__context.log(
                f"Error : {result.status_code} - {result.content.decode('utf-8')}", severity=LogLevel.ERROR
            )
            return False
        self.__context.log("Successfully updated page.")
        return True

    def __get_page_version(self, page_id: str) -> Optional[int]:
        """
        Get the current version number of a page.

        Parameters
        ----------
        page_id : str
            ID of the page.

        Returns
        -------
        Optional[int]
            Current version number if successful, None otherwise.
        """
        endpoint = f"{self.__rest_uri}/content/{page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            json_response: Dict[str, Any] = result.json()
            version_number: int = json_response["version"]["number"]
            return version_number
        self.__context.log(f"Could not get the version of page {page_id}:")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8')}")
        return None
