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

    def pin_and_tag_builds(self):
        """Pin and tag the appropriate builds."""
        tag = f"DIMRset_{self.__dimr_version}"

        build_info = self.__teamcity.get_build_info_for_latest_build_for_build_type_id(
            TEAMCITY_IDS.DIMR_PUBLISH.value
        )
        if build_info is None or "id" not in build_info:
            raise ValueError(
                f"Could not retrieve build info or 'id' from TeamCity for {TEAMCITY_IDS.DIMR_PUBLISH.value}."
            )

        self.__teamcity.add_tag_to_build_with_dependencies(
            build_id=build_info["id"], tag=tag
        )

        build_ids_to_pin = self.__teamcity.get_filtered_dependent_build_ids(
            build_id=build_info["id"]
        )
        for build_id in build_ids_to_pin:
            self.__teamcity.pin_build(build_id=build_id)
