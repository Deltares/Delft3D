import argparse
import re
from dataclasses import dataclass

import requests

# Configuration
BASE_URL = "https://containers.deltares.nl/api/v2.0/projects/delft3d/repositories/delft3dfm/artifacts"
PAGE_SIZE = 100  # Maximum page size for Harbor API


@dataclass(order=True)
class SemVer:
    """Represents a semantic version tag (e.g., 2.30.06-development)."""

    major: int
    minor: int
    patch: int
    tag: str


def parse_semver_tags(tags: list[str]) -> list[SemVer]:
    """
    Parse tags matching semantic versioning format (e.g., 2.30.06-development, weekly-2.29.23).

    Args:
        tags: List of tag strings

    Returns
    -------
        List of SemVer objects sorted by version descending
    """
    semver_pattern = re.compile(r"^(?:[\w]+-)?(\d+)\.(\d+)\.(\d+)(?:-[\w]+)?$")
    semvers = []

    for tag in tags:
        match = semver_pattern.match(tag)
        if match:
            major = int(match.group(1))
            minor = int(match.group(2))
            patch = int(match.group(3))
            semvers.append(SemVer(major=major, minor=minor, patch=patch, tag=tag))

    semvers.sort(reverse=True)
    return semvers


def fetch_all_tags(username: str, password: str) -> list[str]:
    """
    Fetch all tags from Harbor API artifacts.

    Args:
        username: Harbor API username
        password: Harbor API password

    Returns
    -------
        List of all tag names from all artifacts
    """
    # Fetch all artifacts with pagination
    all_artifacts = []
    page = 1

    while True:
        url = f"{BASE_URL}?page={page}&page_size={PAGE_SIZE}"
        print(f"Fetching page {page}...")
        response = requests.get(url, auth=(username, password), timeout=30)

        if response.status_code != 200:
            print(f"Error: Failed to query API. Status code: {response.status_code}")
            print(f"Response: {response.text}")
            break

        artifacts = response.json()
        if not artifacts:  # No more results
            break

        all_artifacts.extend(artifacts)
        page += 1

    print(f"\nTotal artifacts fetched: {len(all_artifacts)}")

    # Extract all tags from all artifacts
    all_tags = []
    for artifact in all_artifacts:
        if artifact.get("tags"):
            for tag in artifact["tags"]:
                tag_name = tag.get("name")
                if tag_name:
                    all_tags.append(tag_name)

    return all_tags


def parse_arguments() -> argparse.Namespace:
    """
    Parse command line arguments.

    Returns
    -------
        Parsed command line arguments
    """
    parser = argparse.ArgumentParser(description="Check Harbor repository for latest version tags")
    parser.add_argument(
        "--harbor-username",
        type=str,
        required=True,
        help="Harbor API username",
    )
    parser.add_argument(
        "--harbor-password",
        type=str,
        required=True,
        help="Harbor API password",
    )
    parser.add_argument(
        "--new-tag",
        type=str,
        required=True,
        help="New tag to compare against latest versions",
    )
    return parser.parse_args()


def main() -> None:
    """Fetch artifacts from Harbor API and display latest version tags."""
    args = parse_arguments()
    all_tags = fetch_all_tags(args.harbor_username, args.harbor_password)

    # Print the list of tags
    print("\nAll tags found:")
    for tag in sorted(all_tags):
        print(f"  - {tag}")

    print(f"\nTotal tags: {len(all_tags)}")

    # Find latest semver version
    semvers = parse_semver_tags(all_tags)

    latest_semver = semvers[0].tag if semvers else None

    print("\n" + "=" * 60)
    print("LATEST VERSION")
    print("=" * 60)
    print(f"Latest Semver tag: {latest_semver}")

    print("\nAll Semver tags found:")
    if semvers:
        for tag in [sv.tag for sv in semvers]:
            print(f"  - {tag}")
    else:
        print("  None found")

    # Check if new_tag matches or is newer than latest semver and set TeamCity variable
    # Parse the new tag to compare versions
    new_tag_semvers = parse_semver_tags([args.new_tag])
    is_latest = False

    if new_tag_semvers and semvers:
        # Compare: is_latest if new_tag >= latest semver
        is_latest = new_tag_semvers[0] >= semvers[0]
    elif new_tag_semvers:
        # No existing semvers, so new tag is the latest
        is_latest = True

    print(f"New tag: {args.new_tag}")
    print(f"Latest semver: {latest_semver}")
    print(f"Is latest development: {is_latest}")

    # Set TeamCity service message
    print(f"\n##teamcity[setParameter name='is_latest_development' value='{str(is_latest).lower()}']")


if __name__ == "__main__":
    main()
