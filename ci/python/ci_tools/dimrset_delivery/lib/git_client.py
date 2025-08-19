import subprocess
import sys

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface


class GitClient(ConnectionServiceInterface):
    """Class responsible for tagging Git commits.

    Parameters
    ----------
    repo_url : str
        URL of the Git repository.
    username : str
        Git username for authentication.
    password : str
        Git password for authentication.
    """

    def __init__(
        self,
        username: str,
        password: str,
        context: DimrAutomationContext,
    ) -> None:
        self.__username = username
        self.__password = password
        self.__context = context

        self.repo_url = ""
        if context.settings is None or context.settings.delft3d_git_repo != "Undefined":
            self.repo_url = context.settings.delft3d_git_repo

    def tag_commit(self, commit_hash: str, tag_name: str) -> None:
        """Tags a specific commit with a given tag name and pushes the tag to the remote repository.

        Parameters
        ----------
        commit_hash : str
            Hash of the commit to be tagged.
        tag_name : str
            Name of the tag to be created.
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
                    f"##teamcity[message text='Failed to create tag {tag_name} for commit {commit_hash}.' "
                    f"status='ERROR']"
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
                self.__context.log(
                    f"##teamcity[message text='Failed to push tag '{tag_name}' to remote repository; "
                    f"return code: {result.returncode}.' status='ERROR']"
                )
                sys.exit(1)
        except Exception as e:
            self.__context.log(
                f"##teamcity[message text='An error occurred while adding tag to Git: {e}.' status='ERROR']"
            )
            sys.exit(1)

    def test_connection(self, dry_run: bool) -> bool:
        """
        Tests the connection to the Git repository.

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
                return True
            else:
                self.__context.log(
                    (
                        f"##teamcity[message text='Failed to read from the repository; "
                        f"return code {result.returncode}.' status='ERROR']"
                    )
                )
                return False

        except Exception as e:
            self.__context.log(
                f"##teamcity[message text='An error occurred while testing Git connection: {e}.' status='ERROR']"
            )
            return False

    def _get_authenticated_url(self) -> str:
        return self.repo_url.replace("https://", f"https://{self.__username}:{self.__password}@")
