from lib.TeamCity import TeamCity
from settings.teamcity_settings import DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID, \
    DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID, DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID


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

        latest_dimr_collector_release_build_info = self.__teamcity.get_build_info_for_latest_build_for_build_type_id(
            build_type_id=DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID)

        # pin latest 2.DIMR_collector_release
        build_ids_to_pin.append(latest_dimr_collector_release_build_info["id"])

        # pin all dependencies
        for dependency in latest_dimr_collector_release_build_info["artifact-dependencies"]["build"]:
            build_ids_to_pin.append(dependency["id"])

        # pin latest 2.b DIMR_Collector_release_signed
        latest_dimr_collector_release_signed_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
                build_type_id=DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID)
        build_ids_to_pin.append(latest_dimr_collector_release_signed_build_id)

        # pin status of release testbench
        latest_status_of_release_testbench_build_id = self.__teamcity.get_latest_build_id_for_build_type_id(
            build_type_id=DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID)
        build_ids_to_pin.append(latest_status_of_release_testbench_build_id)

        for build_id in build_ids_to_pin:
            self.__pin_and_tag_build(build_id=build_id, tag=tag)

    def __pin_and_tag_build(self, build_id: str, tag: str):
        """ Pins the specified build and adds the specified tag. """
        self.__teamcity.pin_build(build_id=build_id)
        self.__teamcity.add_tag_to_build(build_id=build_id, tag=tag)