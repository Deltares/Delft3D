import re
from argparse import ArgumentParser, Namespace

from httpx import HTTPStatusError

from ci_tools.example_utils.logger import Logger, LogLevel
from ci_tools.teamcity.client import TeamcityClient


def parse_arguments() -> Namespace:
    """Set up argument parser and parse args to namespace.

    Returns
    -------
        Namespace: Parsed arugments
    """
    parser = ArgumentParser(
        description="""Remove unused merge commit tags from teamcity builds.
        Only remove tags that match the regex: ^merge commit ID: [a-z0-9]{40}$
        The access token needs access to the mentioned build configuration or project with the
        following permission scopes:
        [Tag build, View build configuration settings, View build runtime parameters and data]
        """
    )
    parser.add_argument("--server", help="Url of the teamcity server", required=True)
    parser.add_argument("--token", help="Access token to the teamcity server", required=True)
    parser.add_argument(
        "--buildConfiguration",
        help="From which buildConfiguration to delete the tags that are matching the regex",
        required=True,
    )
    parser.add_argument(
        "--apply",
        help="Apply removal of tags. If this is not given it will only list what is going to do",
        required=False,
        default=False,
        action="store_true",
    )
    arguments = parser.parse_args()

    return arguments


if __name__ == "__main__":
    logger = Logger()
    arguments = parse_arguments()

    logger.log(
        message=f"Calling remove tags script with server: {arguments.server} and apply set to {arguments.apply}",
        severity=LogLevel.NORMAL,
    )
    teamcity_client = TeamcityClient.with_bearer_token_auth(arguments.token, arguments.server, False)
    build_configuration = arguments.buildConfiguration
    json_build_tags = teamcity_client.list_tags_on_build(build_configuration)
    number_of_tags = 0
    if len(json_build_tags) == 0:
        logger.log("No tags detected", LogLevel.WARNING)
        exit(1)
    matching_regex_merge_commit_tags = re.compile("^merge commit ID: [a-z0-9]{40}$")
    for tag in json_build_tags:
        number_of_tags += 1
        if matching_regex_merge_commit_tags.match(tag):
            logger.log(f"Tag {tag} matches the regex and will be removed", LogLevel.NORMAL)
            if arguments.apply:
                logger.log(f"Removing tag {tag}")
                payload = f'{{"tag": [{{"name":  "{tag}"}}]}}'
                logger.log(payload)
                try:
                    teamcity_client.remove_tag_from_build(build_configuration, tag)
                except HTTPStatusError as e:
                    logger.log("Apicall triggered a exception")
                    logger.log(f"HTTP Exception for {e.request.url} - {e}")

                logger.log(f"Deleted tag. {tag}.")
        else:
            logger.log(f"Tag {tag} does not match the regex and will be kept in place", LogLevel.NORMAL)
    logger.log(f"Number of tags {number_of_tags}")
