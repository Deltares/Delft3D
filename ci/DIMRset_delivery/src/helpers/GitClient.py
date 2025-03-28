import subprocess
import sys

class GitClient(object):
    """Class responsible for tagging Git commits."""

    def __init__(self, repo_url: str, username: str, password: str):
        self.repo_url = repo_url
        self.__username = username
        self.__password = password

    def tag_commit(self, commit_hash: str, tag_name: str):
        """
        Tags a specific commit with a given tag name and pushes the tag to the remote repository.

        @param commit_hash: Hash of the commit to be tagged.
        @param tag_name: Name of the tag to be created.
        """
        try:
            # Create the tag locally
            result = subprocess.run(
                ['git', 'tag', tag_name, commit_hash],
                capture_output=True, text=True,
                env={"GIT_ASKPASS": "echo", "GIT_USERNAME": self.__username, "GIT_PASSWORD": self.__password}
            )
            if result.returncode != 0:
                print(f"##teamcity[message text='Failed to create tag {tag_name} for commit {commit_hash}.' status='ERROR']")
                sys.exit(1)

            # Push the tag to the remote repository
            auth_repo_url = self.repo_url.replace(
                "https://", f"https://{self.__username}:{self.__password}@"
            )
            result = subprocess.run(
                ["git", "push", "--tags", auth_repo_url],
                capture_output=True,
                text=True,
            )
            
            if result.returncode == 0:
                print(f"Tag '{tag_name}' pushed to remote repository successfully.")
            else:
                print(f"##teamcity[message text='Failed to push tag '{tag_name}' to remote repository; return code: {result.returncode}.' status='ERROR']")
                sys.exit(1)
        except Exception as e:
            print(f"##teamcity[message text='An error occurred while adding tag to Git: {e}.' status='ERROR']")
            sys.exit(1)
    
    def test_connection(self):
        """
        Tests the connection to the remote Git repository.

        """
        try:
            auth_repo_url = self.repo_url.replace(
                "https://", f"https://{self.__username}:{self.__password}@"
            )
            result = subprocess.run(
                ["git", "ls-remote", auth_repo_url], capture_output=True, text=True
            )
            if result.returncode == 0:
                print("Read access to the repository is successful.")
            else:
                print(f"##teamcity[message text='Failed to read from the repository while testing Git connection; return code {result.returncode}.' status='ERROR']")
                sys.exit(1)
        except Exception as e:
            print(f"##teamcity[message text='An error occurred while testing Git connection: {e}.' status='ERROR']")
            sys.exit(1)