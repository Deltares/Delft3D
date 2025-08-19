import json
from types import SimpleNamespace
from typing import Any, Dict, Optional, Union

import requests

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface


class Atlassian(ConnectionServiceInterface):
    """
    Wrapper for the Atlassian Confluence REST API.

    The Confluence REST API uses resource expansion: some parts of
    a resource are not returned unless explicitly specified. This
    simplifies responses and minimizes network traffic.

    For more information about the Atlassian REST API, please see
    the official Atlassian REST API docs at:
    https://developer.atlassian.com/cloud/confluence/rest/intro/
    """

    def __init__(self, username: str, password: str, context: DimrAutomationContext) -> None:
        """
        Instantiate a new Atlassian object.

        Parameters
        ----------
        username : str
            Your Deltares username.
        password : str
            Your Deltares password.
        """
        self.__auth = (username, password)
        self.__base_uri = "https://publicwiki.deltares.nl/"
        self.__rest_uri = f"{self.__base_uri}rest/api/"
        self.__default_headers = {"content-type": "application/json", "accept": "application/json"}
        self.__context = context

    def test_connection(self, dry_run: bool) -> bool:
        """
        Test if the the API connection can successfully be established.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content

        Returns
        -------
        bool
            Returns True if a successful request can be made.
        """
        self.__context.log(f"Checking connection to the Atlassian Confluence API with credentials: {self.__auth[0]}")
        endpoint = f"{self.__rest_uri}content"

        if dry_run:
            self.__context.log(f"GET request: {endpoint}")
            result: Union[SimpleNamespace, requests.Response] = SimpleNamespace(
                status_code=200, content=b"dry-run mock"
            )
        else:
            result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)

        if result.status_code == 200:
            self.__context.log("Successfully connected to the Atlassian Confluence API.")
            return True
        self.__context.log("Could not connect to the Atlassian Confluence API:")
        self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}")
        return False

    def get_page_info_for_parent_page(self, parent_page_id: str) -> Optional[Dict[str, Any]]:
        """
        Get the page info for the given parent page.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<advanced_search>

        Parameters
        ----------
        parent_page_id : str
            The id of the parent page.

        Returns
        -------
        Dict[str, Any] | None
            A dictionary with various keys. The "results" key
            contains an array of dictionaries with child page info.

            For more information, please see the official Atlassian Confluence
            REST API docs.

            Returns None if the request failed.
        """
        endpoint = f"{self.__rest_uri}content/search?cql=parent={parent_page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            json_response: Dict[str, Any] = result.json()
            return json_response
        self.__context.log(f"Could not get page info for page {parent_page_id}:")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8')}")
        return None

    def create_public_wiki_page(
        self, page_title: str, space_id: str, ancestor_id: str, body: str = ""
    ) -> Optional[str]:
        """
        Create a new page on the Public Wiki.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content

        Parameters
        ----------
        page_title : str
            The title for the page.
        space_id : str
            The id for the space the page is in.
        ancestor_id : str
            The id for the parent page.
        body : str
            The content that should be placed on the new page. Defaults to an empty page.

        Returns
        -------
        str | None
            The id for the newly created page.

            Returns None if the request failed.
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

        endpoint = f"{self.__rest_uri}content"
        result = requests.post(
            url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False
        )
        if result.status_code == 200:
            self.__context.log("Successfully created page.")
            json_response: Dict[str, Any] = result.json()
            page_id: str = json_response["id"]
            return page_id
        self.__context.log("Could not create page:")
        self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}")
        return None

    def update_page(self, page_id: str, page_title: str, content: str, next_version: Optional[int] = None) -> bool:
        """
        Update a page on the Public Wiki.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<page_id>

        Parameters
        ----------
        page_id : str
            The id of the page to update.
        page_title : str
            The title to set for the page.
        content : str
            The content to set on the page.
        next_version : int, optional
            The next version number for the page. Defaults to None.

        Returns
        -------
        bool
            Returns true if the page was successfully updated.
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

        endpoint = f"{self.__rest_uri}content/{page_id}"
        result = requests.put(
            url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False
        )
        if result.status_code != 200:
            self.__context.log("Could not update page:")
            self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}")
            return False
        self.__context.log("Successfully updated page.")
        return True

    def __get_page_version(self, page_id: str) -> Optional[int]:
        """
        Get the current version of a page.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<page_id>

        Parameters
        ----------
        page_id : str
            The id of the page to update.

        Returns
        -------
        int | None
            The current version of the page.
        """
        endpoint = f"{self.__rest_uri}content/{page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            json_response: Dict[str, Any] = result.json()
            version_number: int = json_response["version"]["number"]
            return version_number
        self.__context.log(f"Could not get the version of page {page_id}:")
        self.__context.log(f"{result.status_code} - {result.content.decode('utf-8')}")
        return None
