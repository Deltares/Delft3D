import argparse
import fnmatch
import json
import subprocess
from typing import Dict, List, Set

GITLAB_PROJECT_URL = "https://git.deltares.nl/api/v4/projects/14"


class FolderIndex:
    """Check the path of changed files against the component index."""

    def __init__(self) -> None:
        self.repo_index: Dict = dict()
        self.changes_with_component: List[str] = list()

    def __readjson(self, file_path: str) -> None:
        with open(file_path, "r") as file:
            self.repo_index = json.load(file)

    def __get_component(self, changed_file_path: str) -> str:
        calculated: Set[str] = set()
        for component, path_list in self.repo_index.items():
            for path in path_list:
                if fnmatch.fnmatch(changed_file_path, path):
                    calculated.add(component)

        component = list(calculated)[0] if len(calculated) == 1 else None
        self.changes_with_component.append(f"{component or 'all'} : {changed_file_path}")
        return component or "all"

    def get_scope(self, json_dict_path: str, file_changes_list: List[str]) -> str:
        """Determine the scope of the code changes in the merge-request."""
        self.__readjson(json_dict_path)
        components: Set[str] = set()
        for changed_file_path in file_changes_list:
            components.add(self.__get_component(changed_file_path))

        if len(components) == 1:
            component = list(components)[0]
        else:
            component = "all"

        return component


class GitlabApi:
    """Handle gitlab api request to determine changes on merge-request."""

    def __init__(self) -> None:
        self.merge_request_data: Dict = dict()
        self.change_paths: Set[str] = set()

    def __retrieve_merge_details(self, merge_branch: str, token: str) -> None:
        url = f"{GITLAB_PROJECT_URL}/{merge_branch}/changes"
        merge_request_details = subprocess.check_output(["curl", "--header", f"PRIVATE-TOKEN: {token}", url])
        self.merge_request_data = json.loads(merge_request_details)

    def __calculate_file_changes(self) -> None:
        changes = self.merge_request_data.get("changes", [])
        for change in changes:
            self.change_paths.add(change.get("new_path"))
            self.change_paths.add(change.get("old_path"))

    def get_file_changes(self, merge_branch: str, token: str) -> List[str]:
        """Use the gitlab api to retrieve the code changes and return the changed files as list."""
        self.__retrieve_merge_details(merge_branch, token)
        self.__calculate_file_changes()
        return list(self.change_paths)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Check the scope of changes made in the merge-request.")

    # Add arguments
    parser.add_argument(
        "-b", type=str, dest="merge_branch", required=True, help="The merge-request branch to check changes on."
    )
    parser.add_argument(
        "-t", type=str, dest="token", required=True, help="The private access token to access the gitlab API."
    )
    parser.add_argument(
        "-f", type=str, dest="file", required=True, help="The location of the repo folder definition json."
    )

    args = parser.parse_args()
    merge_branch: str = args.merge_branch
    token: str = args.token
    json_file: str = args.file

    # The gitlab api requires _ instead of the - of the git branch.
    merge_branch = merge_branch.replace("merge-requests", "merge_requests")

    api = GitlabApi()
    changes = api.get_file_changes(merge_branch, token)

    indexer = FolderIndex()
    component = indexer.get_scope(json_file, changes)

    changes_string = "\n".join(indexer.changes_with_component)
    print(f"Determined scope: {component}\n----------- CHANGES -----------\n{changes_string}")
    print(f"##teamcity[setParameter name='branch_name' value='{component}']")
