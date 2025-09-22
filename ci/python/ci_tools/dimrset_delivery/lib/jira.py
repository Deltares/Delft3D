from types import SimpleNamespace
from typing import Any, Dict, Optional, Union, cast

import requests

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface


class Jira(ConnectionServiceInterface):
    """
    Atlassian Jira REST API wrapper.

    Provides methods to interact with the Jira API, including connection testing and fetching issue details.

    Usage:
        client = Jira(credentials, context)
        issue = client.get_issue("DEVOPSDSC-123")
    """

    def __init__(self, credentials: Credentials, context: DimrAutomationContext) -> None:
        """
        Initialize Jira API client.

        Parameters
        ----------
        credentials : Credentials
            API bearer token for authentication.
        context : DimrAutomationContext
            Automation context for logging and configuration.
        """
        self.__auth = credentials.password
        self.__rest_uri = "https://issuetracker.deltares.nl/rest/api/latest"
        self.__default_headers = {
            "Authorization": f"Bearer {self.__auth}",
            "Accept": "application/json",
            "Content-Type": "application/json",
        }
        self.__context = context

    def test_connection(self) -> bool:
        """
        Test API connection to Jira.

        Returns
        -------
        bool
            True if connection is successful, False otherwise.
        """
        self.__context.log("Checking connection to Jira...")
        endpoint = f"{self.__rest_uri}/myself"

        if self.__context.dry_run:
            self.__context.log(f"GET request: {endpoint}")
            result: Union[SimpleNamespace, requests.Response] = SimpleNamespace(
                status_code=200, content=b"dry-run mock"
            )
        else:
            result = requests.get(url=endpoint, headers=self.__default_headers, verify=True, timeout=(5, 30))

        if result.status_code == 200:
            self.__context.log("Successfully connected to the Jira API.")
            return True

        self.__context.log("Could not connect to the Jira API:")
        self.__context.log(f"Error : {result.status_code} - {result.content.decode('utf-8')}")
        return False

    def get_issue(self, issue_number: str) -> Optional[Dict[str, Any]]:
        """
        Fetch details for a Jira issue.

        Parameters
        ----------
        issue_number : str
            The Jira issue number, e.g. DEVOPSDSC-123.

        Returns
        -------
        Optional[Dict[str, Any]]
            Issue details if successful, None otherwise.
        """
        endpoint = f"{self.__rest_uri}/issue/{issue_number}"

        if self.__context.dry_run:
            self.__context.log(f"GET request: {endpoint}")
            return {"key": issue_number, "fields": {"summary": f"[dry-run mock] Summary for {issue_number}"}}

        result = requests.get(url=endpoint, headers=self.__default_headers, verify=True, timeout=(5, 30))

        if result.status_code == 200:
            return cast(Dict[str, Any], result.json())

        self.__context.log(
            f"Could not fetch issue {issue_number}: {result.status_code} - {result.content.decode('utf-8')}"
        )
        return None
