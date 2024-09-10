import subprocess

class GitClient(object):
    """ Class responsible for tagging Git commits. """

    def __init__(self, repo_url: str, username: str, password: str):
        self.repo_url = repo_url
        self.__username = username
        self.__password = password

    def tag_commit(self, commit_hash: str, tag_name: str):
        """
        Tags a specific commit with a given tag name and pushes the tag to the remote repository.

        @param commit_hash: Hash of the commit to be tagged.
        @param tag_name: Name of the tag to be created.
        @return: True if the tag was created and pushed successfully, False otherwise.
        """
        try:
            # Create the tag locally
            result = subprocess.run(
                ['git', 'tag', tag_name, commit_hash],
                capture_output=True, text=True,
                env={"GIT_ASKPASS": "echo", "GIT_USERNAME": self.__username, "GIT_PASSWORD": self.__password}
            )
            if result.returncode != 0:
                print(f"Failed to create tag '{tag_name}' for commit {commit_hash}.")
                return False

            # Push the tag to the remote repository
            result = subprocess.run(
                ['git', 'push', '--tags'],
                capture_output=True, text=True,
            )
            
            if result.returncode == 0:
                print(f"Tag '{tag_name}' pushed to remote repository successfully.")
                return True
            else:
                print(f"Failed to push tag '{tag_name}' to remote repository.")
                return False
        except Exception as e:
            print(f"An error occurred: {e}")
            return False
    
    def test_connection(self) -> bool:
        """
        Tests the connection to the remote Git repository.

        @return: True if the connection is successful, False otherwise.
        """
        try:
            result = subprocess.run(['git', 'ls-remote', self.repo_url], capture_output=True, text=True)
            if result.returncode == 0:
                print("Read access to the repository is successful.")
                return True
            else:
                print("Failed to read from the repository.")
                return False
        except Exception as e:
            print(f"An error occurred: {e}")
            return False