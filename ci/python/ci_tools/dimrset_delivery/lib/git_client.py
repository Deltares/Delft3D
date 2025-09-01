import hashlib
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
        self.__credentials = credentials
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
            if self.__context.dry_run:
                result = subprocess.CompletedProcess(args=[], returncode=0, stdout=f"git tag {tag_name} {commit_hash}")
            else:
                result = subprocess.run(
                    ["git", "tag", tag_name, commit_hash],
                    capture_output=True,
                    text=True,
                    env={
                        "GIT_ASKPASS": "echo",
                        "GIT_USERNAME": self.__credentials.username,
                        "GIT_PASSWORD": self.__credentials.password,
                    },
                )
            if result.returncode != 0:
                self.__context.log(result.stderr)
                self.__context.log(
                    f"Failed to create tag {tag_name} for commit {commit_hash}.", severity=LogLevel.ERROR
                )
                sys.exit(1)
            else:
                self.__context.log(result.stdout)

            # Push the tag to the remote repository
            auth_repo_url = self._get_authenticated_url()

            if self.__context.dry_run:
                result = subprocess.CompletedProcess(args=[], returncode=0, stdout=f"git push --tags {self.repo_url}")
            else:
                result = subprocess.run(
                    ["git", "push", "--tags", auth_repo_url],
                    capture_output=True,
                    text=True,
                )

            if result.returncode == 0:
                self.__context.log(result.stdout)
                self.__context.log(f"Tag '{tag_name}' pushed to remote repository successfully.")
            else:
                self.__context.log(result.stderr)
                self.__context.log(f"Failed to push tag '{tag_name}' to remote repository", severity=LogLevel.ERROR)
                sys.exit(1)
        except Exception as e:
            self.__context.log(f"An error occurred while adding tag to Git: {e}.", severity=LogLevel.ERROR)
            sys.exit(1)

    def test_connection(self) -> bool:
        """Test the connection to the Git repository.

        Returns
        -------
        bool
            True if the connection test is successful, False otherwise.
        """
        auth_repo_url = self._get_authenticated_url()

        try:
            if self.__context.dry_run:
                self.__context.log(f"Testing connection to {auth_repo_url}")
                result = subprocess.CompletedProcess(args=[], returncode=0, stdout=f"git ls-remote {self.repo_url}")
            else:
                result = subprocess.run(["git", "ls-remote", auth_repo_url], capture_output=True, text=True)
                self.__context.log(f"git ls-remote using username '{self.__credentials.username}'.")
                hashed_pw = hashlib.md5(self.__credentials.password.encode()).hexdigest()
                self.__context.log(f"Using password (md5 hashed): '{hashed_pw}'.")

            if result.returncode == 0:
                self.__context.log(result.stdout)
                self.__context.log("Read access to the repository is successful.")
                success = True
            else:
                self.__context.log(result.stderr)
                self.__context.log(
                    f"Failed to read from the repository return code {result.returncode}.", severity=LogLevel.ERROR
                )
                success = False

            return success

        except Exception as e:
            self.__context.log(f"An error occurred while testing Git connection: {e}.", severity=LogLevel.ERROR)
            return False

    def _get_authenticated_url(self) -> str:
        return self.repo_url.replace(
            "https://", f"https://{self.__credentials.username}:{self.__credentials.password}@"
        )
