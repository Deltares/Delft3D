import os
from dataclasses import dataclass
from enum import Enum
from getpass import getpass
from typing import Dict, Iterator, Optional

from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.example_utils.logger import Logger, LogLevel


class ServiceName(str, Enum):
    """Enum representing application names for DIMR automation."""

    JIRA = "Jira"
    TEAMCITY = "TeamCity"
    SSH = "SSH"
    GIT = "Git"


@dataclass
class Credentials:
    """
    Stores username and password for a service.

    Attributes
    ----------
    username : str
        The username for the service.
    password : str
        The password for the service.
    """

    username: str
    password: str


class CredentialEntry:
    """
    Represents a credential entry for a specific service.

    Attributes
    ----------
    name : ServiceName
        The name of the service.
    required : bool
        Whether the credential is required.
    credential : Credentials
        The credentials for the service.
    """

    def __init__(
        self,
        required: bool,
        credential: Credentials,
    ) -> None:
        self.required = required
        self.credential = credential


class ServiceAuthenticateStore:
    """
    Stores credentials for DIMR automation services.

    This dataclass holds usernames and passwords for all supported external services.
    """

    def __init__(self) -> None:
        self.__credentials: Dict[ServiceName, CredentialEntry] = {}

    def add(self, service: ServiceName, entry: CredentialEntry) -> None:
        """
        Add or update credentials for a specified service.

        If an entry for the service already exists, it will be replaced.

        Parameters
        ----------
        service : ServiceName
            The service for which credentials are being added.
        entry : CredentialEntry
            The credential entry to add or update.
        """
        self.__credentials[service] = entry

    def get(self, service: ServiceName) -> Optional[CredentialEntry]:
        """
        Retrieve credentials for a specified service.

        Parameters
        ----------
        service : str or ServiceName
            The name of the service for which credentials are requested.

        Returns
        -------
        Optional[Credentials]
            The credentials for the specified service, or None if not found.
        """
        return self.__credentials.get(service, None)

    def __iter__(self) -> Iterator[tuple[ServiceName, CredentialEntry]]:
        """Allow iteration over all (service, credential entry) pairs."""
        return iter(self.__credentials.items())


class DimrAutomationContext:
    """
    Shared context for DIMR automation steps.

    Provides access to credentials, requirements, settings, and cached data for automation scripts.
    """

    def __init__(
        self,
        build_id: str,
        dry_run: bool = False,
        credentials: Optional[ServiceAuthenticateStore] = None,
        teamcity_logger: bool = False,
    ) -> None:
        """
        Initialize DIMR automation context.

        Parameters
        ----------
        build_id : str
            The TeamCity build ID.
        dry_run : bool, optional
            Whether to run in dry-run mode. Default is False.
        credentials : CredentialsStore, optional
            Credentials store for various services. Default is a new CredentialsStore.
        teamcity_logger : bool, optional
            Whether to use TeamCity-specific logging. Default is False.
        """
        self.build_id = build_id
        self.dry_run = dry_run

        # Initialize credentials store if not provided
        if credentials is None:
            credentials = ServiceAuthenticateStore()

        self.credentials = credentials
        self._prompt_for_missing_credentials(credentials)

        settings_path = os.path.join(os.path.dirname(__file__), "settings", "teamcity_settings.json")
        self.settings = Settings(settings_path)

        # Cache for commonly needed data
        self.kernel_versions: Dict[str, str] = {}
        self.dimr_version: str = ""
        self.branch_name: str = ""
        self.logger = Logger(teamcity_logger)

    def log(self, *args: object, sep: str = " ", severity: LogLevel = LogLevel.NORMAL) -> None:
        """
        Print status message with dry-run prefix if applicable.

        Parameters
        ----------
        args : object
            Objects to print.
        sep : str, optional
            Separator between objects. Default is a space.
        """
        message = f"{sep.join(str(arg) for arg in args)}"
        if self.dry_run:
            message = f"{self.settings.dry_run_prefix}{sep}{message}"

        self.logger.log(message, severity)

    def _prompt_for_missing_credentials(self, credentials: ServiceAuthenticateStore) -> None:
        """
        Prompt for any missing required credentials and validate presence after prompting.

        Parameters
        ----------
        credentials : CredentialsStore
            Credentials object to fill in.

        Raises
        ------
        ValueError
            If any required credentials are missing after prompting.
        """
        for service, credential in credentials:
            new_credential = self.__prompt_credentials_if_not_yet_provided(service, credential)
            if new_credential:
                credentials.add(service, new_credential)

    def __prompt_credentials_if_not_yet_provided(
        self,
        service: ServiceName,
        credential_entry: CredentialEntry,
    ) -> Optional[CredentialEntry]:
        if credential_entry is None or (
            credential_entry.required
            and (not credential_entry.credential.password or not credential_entry.credential.username)
        ):
            username = input(f"Enter your {service} username:")
            password = getpass(prompt=f"Enter your {service} password:", stream=None)
            if username == "" or password == "":
                raise ValueError(f"{service.value} credentials are required but not provided")
            return CredentialEntry(required=True, credential=Credentials(username, password))
        return None
