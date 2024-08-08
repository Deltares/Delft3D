import json
import requests
from typing import List, Dict, Tuple, Any, IO


class Atlassian(object):
    """
    Wrapper for the Atlassian Confluence REST API.

    The Confluence REST API uses resource expansion: some parts of 
    a resource are not returned unless explicitly specified. This 
    simplifies responses and minimizes network traffic.

    For more information about the Atlassian REST API, please see 
    the official Atlassian REST API docs at:
    https://developer.atlassian.com/cloud/confluence/rest/intro/
    """

    def __init__(self, username: str, password: str):
        """
        Instantiates a new Atlassian object.

        Args:
            username (str): Your Deltares username.
            password (str): Your Deltares password.
        """
        self.__auth = (username, password)
        self.__base_uri = "https://publicwiki.deltares.nl/"
        self.__rest_uri = f"{self.__base_uri}rest/api/"
        self.__default_headers = {
            "content-type": "application/json",
            "accept": "application/json"
        }

    def test_api_connection(self) -> bool:
        """
        Tests if the the API connection can successfully 
        be established.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content

        Returns:
            bool: Returns True if a successful request can be made.
        """
        endpoint = f"{self.__rest_uri}content"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            return True
        print(f"Could not connect to the Atlassian Confluence API:")
        print(f"Error : {result.status_code} - {result.content}")
        return False

    def get_page_info_for_parent_page(self, parent_page_id: str) -> Dict[str, Any]:
        """
        Gets the page info for the given parent page.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<advanced_search>

        Arguments:
            parent_page_id (str): The id of the parent page.

        Returns:
            Dict[str, Any]: A dictionary with various keys. The "results" key
            contains an array of dictionaries with child page info.
            
            For more information, please see the official Atlassian Confluence
            REST API docs.

            Returns None if the request failed.
        """
        endpoint = f"{self.__rest_uri}content/search?cql=parent={parent_page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            return result.json()
        print(f"Could not get page info for page {parent_page_id}:")
        print(f"{result.status_code} - {result.content}")
        return None

    def create_public_wiki_page(self, page_title: str, space_id: str, ancestor_id: str, body: str = "") -> str:
        """
        Creates a new page on the Public Wiki.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content

        Arguments:
            page_title (str): The title for the page.
            space_id (str): The id for the space the page is in.
            ancestor_id (str): The id for the parent page.
            body (str): The content that should be placed on
            the new page. Defaults to an empty page.

        Returns:
            str: The id for the newly created page.

            Returns None if the request failed.
        """
        payload = json.dumps({
            "title": page_title,
            "type": "page",
            "space": {
                "key": space_id
            },
            "status": "current",
            "ancestors": [
                {
                    "id": ancestor_id
                }
            ],
            "body": {
                "storage": {
                    "value": str(body),
                    "representation": "storage"
                }
            }
        })

        endpoint = f"{self.__rest_uri}content"
        result = requests.post(url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False)
        if result.status_code == 200:
            print("Successfully created page.")
            return result.json()["id"]
        print("Could not create page:")
        print(f"Error : {result.status_code} - {result.content}")
        return None

    def update_page(self, page_id: str, page_title: str, content: str, next_version: int = None) -> bool:
        """
        Updates a page on the Public Wiki.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<page_id>

        Args:
            page_id (str): The id of the page to update.
            page_title (str): The title to set for the page.
            content (str): The content to set on the page.
            next_version (int): The next version number for the page. Defaults to None.

        Returns:
            bool: Returns true if the page was successfully updated.
        """
        if next_version is None:
            current_version = self.get_page_version(page_id)
            next_version = current_version + 1
            if next_version is None:
                print("Could not update page:")
                print("Could not retrieve the current version of the page.")

        payload = json.dumps({
            "version": {
                "number": next_version
            },
            "title": page_title,
            "type": "page",
            "status": "current",
            "body": {
                "storage": {
                    "value": str(content),
                    "representation": "storage"
                }
            }
        })

        endpoint = f"{self.__rest_uri}content/{page_id}"
        result = requests.put(url=endpoint, headers=self.__default_headers, auth=self.__auth, data=payload, verify=False)
        if result.status_code != 200:
            print("Could not update page:")
            print(f"Error : {result.status_code} - {result.content}")
            return False
        print("Successfully updated page.")
        return True

    def get_page_version(self, page_id: str):
        """
        Gets the current version of a page.

        Uses the following Atlassian REST API endpoint:
        /rest/api/content/<page_id>

        Args:
            page_id (str): The id of the page to update.

        Returns:
            str: The current version of the page.
        """
        endpoint = f"{self.__rest_uri}content/{page_id}"
        result = requests.get(url=endpoint, headers=self.__default_headers, auth=self.__auth, verify=False)
        if result.status_code == 200:
            return result.json()["version"]["number"]
        print(f"Could not get the version of page {page_id}:")
        print(f"{result.status_code} - {result.content}")
        return None
