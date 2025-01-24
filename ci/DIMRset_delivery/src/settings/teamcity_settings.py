from helpers.KernelData import KernelData
from helpers.ToolData import ToolData
from enum import Enum

# All kernels for which the versions are set in the configuration parameters of 2.Dimr_collector_release
KERNELS = [
    KernelData(name_for_extracting_revision="DIMRset_ver", name_for_email="DIMRset"),
    KernelData(name_for_extracting_revision="OSS_ver", name_for_email="OSS"),
    KernelData(name_for_extracting_revision="RTCTools_ver", name_for_email="FBC"),

]

# All tools for which a version should be extracted from the Version.txt artifact for creating the SVN log message
TOOLS = [
    ToolData(name_for_extracting_version="D-Flow FM", name_for_svn_log="D-Flow FM"),
    ToolData(name_for_extracting_version="WQ PROCESSES", name_for_svn_log="D-WAQ"),
    ToolData(name_for_extracting_version="Delft3D-WAVE", name_for_svn_log="D-Waves"),
    ToolData(name_for_extracting_version="DIMR_EXE", name_for_svn_log="DIMR")
]

# All testcase groups that are listed in the Status of Release Testbench artifact
TESTCASE_GROUPS = [
    "D-Flow FM",
    "D-Flow FM, D-RR",
    "D-Flow FM, D-RTC",
    "D-Flow FM, D-RTC, D-Waves",
    "D-Flow FM, D-Waves",
    "D-PART",
    "D-RR",
    "D-WAQ",
    "D-Waves",
]

class TEAMCITY_IDS(Enum):
    DIMR_COLLECTOR_RELEASE_BUILD_TYPE_ID = "Dimr_DimrCollector"
    DIMR_COLLETOR_RELEASE_SIGNED_BUILD_TYPE_ID = "Dimr_DimrCollectors_2bDimrCollectorReleaseSigned"
    DIMR_TO_NGHS_BUILD_TYPE_ID = "DIMR_To_NGHS" 
    DIMR_TESTBENCH_RELEASE_BUILD_TYPE_ID = "Dimr_DimrTestbenchRelease_StatusOfDailyTestbench"
    DIMR_TESTBENCH_RELEASE_TESTS_LINUX = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsLinux"
    DIMR_TESTBENCH_RELEASE_TESTS_WINDOWS = "Dimr_DimrCollectors_DIMRsetAggregatedReleaseResultsWindows"

# Path to Windows version artifact on TeamCity
PATH_TO_WINDOWS_VERSION_ARTIFACT = "version/dimr_version_release_x64.txt"

# Path to Linux version artifact on TeamCity
PATH_TO_LINUX_VERSION_ARTIFACT = "version/dimr_version_release_lnx64.txt"

# Path to release test results artifact on TeamCity
PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT = "teamcity_retrieve_release_engine_test_status.txt"

# Path to summary of DIMR collector release signed artifact on TeamCity
PATH_TO_DIMR_COLLECTOR_RELEASE_SIGNED_ARTIFACT = "signed/summary.txt"

# Name of the DIMR set release signed Linux artifact
NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT = "dimrset_lnx64"

# Name of the DIMR set release signed Windows artifact
NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT = "dimrset_x64_signed"
