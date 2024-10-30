from lib.TeamCity import TeamCity
from settings.teamcity_settings import DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID, \
    DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID, DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID, \
    DIMR_TO_NGHS_BUILD_TYPE_ID, DIMR_TESTBENCH_RELEASE_TESTS_LINUX, DIMR_TESTBENCH_RELEASE_TESTS_WINDOWS
    


class PinHelper(object):
    """ Class responsible for pinning and tagging builds in TeamCity. """

    def __init__(self, teamcity: TeamCity, dimr_version: str):
        """
        Creates a new instance of PinHelper.
        """
        self.__teamcity = teamcity
        self.__dimr_version = dimr_version

    def pin_and_tag_builds(self):
        """ Pin and tag the appropriate builds. """
        tag = f"DIMRset_{self.__dimr_version}"

        build_ids_to_pin = []

        build_type_ids = [
            DIMR_TO_NGHS_BUILD_TYPE_ID,
            DIMR_TESTBENCH_RELEASE_TESTS_LINUX,
            DIMR_TESTBENCH_RELEASE_TESTS_WINDOWS,
            DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID,
            DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID,
            DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID,
        ]

        for build_type_id in build_type_ids:
            build_info = self.__teamcity.get_build_info_for_latest_build_for_build_type_id(build_type_id)
            build_ids_to_pin.append(build_info["id"])
            if build_type_id == DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID:
                for dependency in build_info["artifact-dependencies"]["build"]:
                    build_ids_to_pin.append(dependency["id"])

        for build_id in build_ids_to_pin:
            self.__pin_and_tag_build(build_id=build_id, tag=tag)

    def __pin_and_tag_build(self, build_id: str, tag: str):
        """ Pins the specified build and adds the specified tag. """
        self.__teamcity.pin_build(build_id=build_id)
        self.__teamcity.add_tag_to_build(build_id=build_id, tag=tag)