import os
from dataclasses import dataclass
from getpass import getpass
from typing import Dict, Iterator, Optional

from pyparsing import Enum

from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.example_utils.logger import Logger, LogLevel


class ServiceName(str, Enum):
    """Enum representing application names for DIMR automation."""

    ATLASSIAN = "Atlassian"
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


@dataclass
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
        name: ServiceName,
        required: bool,
        credential: Credentials,
    ) -> None:
        self.name = name
        self.required = required
        self.credential = credential


class ServiceAuthenticateStore:
    """
    Stores credentials for DIMR automation services.

    This dataclass holds usernames and passwords for all supported external services.
    """

    def __init__(self) -> None:
        self.__credentials: list[CredentialEntry] = []

    def add(self, entry: CredentialEntry) -> None:
        """
        Add or update credentials for a specified service.

        If an entry for the service already exists, it will be replaced.

        Parameters
        ----------
        entry : CredentialEntry
            The credential entry to add or update.
        """
        for i, existing_entry in enumerate(self.__credentials):
            if existing_entry.name == entry.name:
                self.__credentials[i] = entry
                return
        self.__credentials.append(entry)

    def get(self, service: ServiceName) -> Optional[CredentialEntry]:
        """
        Retrieve credentials for a specified service.

        Parameters
        ----------
        service : str
            The name of the service for which credentials are requested.

        Returns
        -------
        Optional[Credentials]
            The credentials for the specified service, or None if not found.
        """
        for entry in self.__credentials:
            if entry.name == service:
                return entry
        return None

    def __iter__(self) -> Iterator[CredentialEntry]:
        """Allow iteration over all credential entries."""
        return iter(self.__credentials)


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
        for credential in credentials:
            new_credential = self.__prompt_credentials_if_not_yet_provided(credential, credential.name)
            if new_credential:
                credentials.add(new_credential)

    def __prompt_credentials_if_not_yet_provided(
        self,
        credential_entry: CredentialEntry,
        service: ServiceName,
    ) -> Optional[CredentialEntry]:
        if credential_entry is None or (
            credential_entry.required
            and (not credential_entry.credential.password or not credential_entry.credential.username)
        ):
            username = input(f"Enter your {service} username:")
            password = getpass(prompt=f"Enter your {service} password:", stream=None)
            if username == "" or password == "":
                raise ValueError(f"{service.value} credentials are required but not provided")
            return CredentialEntry(name=service, required=True, credential=Credentials(username, password))
        return None
