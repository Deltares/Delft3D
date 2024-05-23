from pathlib import Path
from enum import Enum
from typing import Optional
from minio.credentials.providers import AWSConfigProvider
from src.utils.logging.log_level import LogLevel
from src.utils.logging.console_logger import ConsoleLogger
from src.config.credentials import Credentials


class CredentialStatus(Enum):
    """Enum for the credentials status."""
    MISSING = 0
    PASSWORD = 1
    USERNAME = 2
    BOTH = 3


class CredentialHandler:
    """Handle credential file and setup of credential file for local use."""

    def __init__(self, credentials: Optional[Credentials] = None, profile: Optional[str] = None):
        self.cred_path: Path = (Path("~") / ".aws" / "credentials").expanduser()
        self.credentials: Credentials = credentials
        self.logger: ConsoleLogger = ConsoleLogger(log_level=LogLevel.INFO)
        self.profile: str = profile

    def setup_credentials(self, is_interactive: bool):
        """Handle aws credential file and give warnings/errors for further handling."""
        status = self.credential_status_detect()

        if status == CredentialStatus.BOTH:
            self.logger.warning("Using commandline credentials!")
            if self.cred_path.exists():
                return
            if is_interactive:
                make_file = input("\n".join(
                    [
                        "Credentials were provided and no credential file was located in your home directory.",
                        "Do you wish to create a credential file (yes/no)?"
                    ]))
                if make_file.lower() in ["yes", "y"]:
                    self.setup_file()
                    return
            self.credentials_instruction()
            return

        if status == CredentialStatus.PASSWORD or status == CredentialStatus.USERNAME:
            provided = 'password' if status == CredentialStatus.PASSWORD else 'username'
            self.logger.warning(f"Only {provided} was provided on the commandline.")
            if self.cred_path.exists():
                self.logger.warning("Falling back on credential file.")
            else:
                self.logger.error(f"No credential file found attempting run with only {provided}.")
                self.credentials_instruction()
                return

        if status == CredentialStatus.MISSING and not self.cred_path.exists():
            self.logger.error("No credentials provided and no credential file is available.")
            self.credentials_instruction()
            return

        self.get_credentials()

    def get_credentials(self):
        """Get credentials from file."""
        provider = AWSConfigProvider(
            filename=str(self.cred_path),
            profile=self.profile
        )
        if self.credentials is not None:
            credentials = provider.retrieve()
            self.credentials.username = credentials.access_key
            self.credentials.password = credentials.secret_key
        return provider

    def credential_file_exists(self):
        """Find credential file in home directory and return if exist."""
        return self.cred_path.exists()

    def credential_status_detect(self):
        """Detect which values exist within the credentials."""
        if self.credentials.username != "" and self.credentials.password != "":
            return CredentialStatus.BOTH
        if self.credentials.username == "" and self.credentials.password != "":
            return CredentialStatus.PASSWORD
        if self.credentials.username != "" and self.credentials.password == "":
            return CredentialStatus.USERNAME
        if self.credentials.username == "" and self.credentials.password == "":
            return CredentialStatus.MISSING

    def credentials_instruction(self):
        """Print instructions for setting up a credential file within the home directory."""
        self.logger.warning(
            "\n".join(
                [
                    f"To authenticate, create a 'credentials' file in your home directory: {self.cred_path.expanduser()}",
                    "Example content of the credentials file:",
                    "[default]",
                    "aws_access_key_id = $ACCESS_KEY_ID",
                    "aws_secret_access_key = $SECRET_ACCESS_KEY",
                    "",
                    "Where $ACCESS_KEY_ID and $SECRET_ACCESS_KEY are your personal access and secret keys.",
                    "Create your own personal keys for MinIO at: https://s3-console.deltares.nl",
                    "Details: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html",
                ]
            )
        )

    def setup_file(self):
        """Setup credential file using credentials for keys."""
        with self.cred_path.open('x') as file:
            file.write("[default]\n")
            file.write(f"aws_access_key_id = {self.credentials.username}\n")
            file.write(f"aws_secret_access_key = {self.credentials.password}\n")
        self.logger.info(f"Created a credential file with the given credentials here: {self.cred_path.expanduser()}")
