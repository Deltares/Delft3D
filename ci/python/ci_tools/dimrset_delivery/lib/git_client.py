import subprocess
import sys

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface
from ci_tools.dimrset_delivery.settings.teamcity_settings import INIT_VALUE
from ci_tools.example_utils.logger import LogLevel


class GitClient(ConnectionServiceInterface):
    """Handles Git operations such as tagging commits and testing repository connections."""

    def __init__(
        self,
        credentials: Credentials,
        context: DimrAutomationContext,
    ) -> None:
        """Set up the git client.

        Parameters
        ----------
        credentials : Credentials
            Username and Password for authentication.
        context : DimrAutomationContext
            Context object containing settings and logging utilities.
        """
        self.__username = credentials.username
        self.__password = credentials.password
        self.__context = context

        self.repo_url = ""
        if context.settings is None or context.settings.delft3d_git_repo != INIT_VALUE:
            self.repo_url = context.settings.delft3d_git_repo

    def tag_commit(self, commit_hash: str, tag_name: str) -> None:
        """Tag a commit and push the tag to the remote repository.

        Parameters
        ----------
        commit_hash : str
            Hash of the commit to be tagged.
        tag_name : str
            Name of the tag to be created.

        Raises
        ------
        SystemExit
            If tagging or pushing the tag fails.
        """
        try:
            # Create the tag locally
            result = subprocess.run(
                ["git", "tag", tag_name, commit_hash],
                capture_output=True,
                text=True,
                env={"GIT_ASKPASS": "echo", "GIT_USERNAME": self.__username, "GIT_PASSWORD": self.__password},
            )
            if result.returncode != 0:
                self.__context.log(
                    f"Failed to create tag {tag_name} for commit {commit_hash}.", severity=LogLevel.ERROR
                )
                sys.exit(1)

            # Push the tag to the remote repository
            auth_repo_url = self.repo_url.replace("https://", f"https://{self.__username}:{self.__password}@")
            result = subprocess.run(
                ["git", "push", "--tags", auth_repo_url],
                capture_output=True,
                text=True,
            )

            if result.returncode == 0:
                self.__context.log(f"Tag '{tag_name}' pushed to remote repository successfully.")
            else:
                self.__context.log(f"Failed to push tag '{tag_name}' to remote repository", severity=LogLevel.ERROR)
                sys.exit(1)
        except Exception as e:
            self.__context.log(f"An error occurred while adding tag to Git: {e}.", severity=LogLevel.ERROR)
            sys.exit(1)

    def test_connection(self, dry_run: bool = False) -> bool:
        """Test the connection to the Git repository.

        Parameters
        ----------
        dry_run : bool
            If True, performs a dry run without making actual network requests.

        Returns
        -------
        bool
            True if the connection test is successful, False otherwise.
        """
        auth_repo_url = self._get_authenticated_url()

        try:
            if dry_run:
                self.__context.log(f"Testing connection to {auth_repo_url}")
                result = subprocess.CompletedProcess(args=[], returncode=0, stdout="Dry run successful")
            else:
                result = subprocess.run(["git", "ls-remote", auth_repo_url], capture_output=True, text=True)

            if result.returncode == 0:
                self.__context.log("Read access to the repository is successful.")
                success = True
            else:
                self.__context.log(
                    f"Failed to read from the repository return code {result.returncode}.", severity=LogLevel.ERROR
                )
                success = False

            return success

        except Exception as e:
            self.__context.log(f"An error occurred while testing Git connection: {e}.", severity=LogLevel.ERROR)
            return False

    def _get_authenticated_url(self) -> str:
        return self.repo_url.replace("https://", f"https://{self.__username}:{self.__password}@")
