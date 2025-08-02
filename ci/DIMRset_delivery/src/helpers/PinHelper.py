from lib.TeamCity import TeamCity
from settings.teamcity_settings import TEAMCITY_IDS


class PinHelper(object):
    """Class responsible for pinning and tagging builds in TeamCity."""

    def __init__(self, teamcity: TeamCity, dimr_version: str):
        """
        Creates a new instance of PinHelper.
        """
        self.__teamcity = teamcity
        self.__dimr_version = dimr_version

    def pin_and_tag_builds(self, build_id_chain: str) -> None:
        """Tag all builds and pin the appropriate builds."""
        tag = f"DIMRset_{self.__dimr_version}"

        self.__teamcity.add_tag_to_build_with_dependencies(build_id_chain, tag=tag)

        # Only pin specific builds
        build_ids_to_pin = self.__teamcity.get_filtered_dependent_build_ids(
            build_id_chain
        )
        for build_id in build_ids_to_pin:
            self.__teamcity.pin_build(build_id=build_id)
