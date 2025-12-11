# DIMR Set Delivery Automation

This module contains Python scripts that automate several steps in the weekly DIMR release process. The automation is limited to the latest release only and cannot be used for releases from previous weeks, as it looks for information in the latest successful DIMR release builds.

## Purpose

Every week there is a release of DIMR. The release process includes many manual steps and is very error prone as a lot of numbers have to be entered manually in different places. This release process takes a considerable amount of time each week. These scripts were created to both save time and make the entire process less prone to errors, as all required numbers are extracted and entered automatically.

## Getting Started

To use the DIMR Set Delivery automation tools, you need to install the package with the `dimrset_delivery` optional dependencies:

```bash
# From the ci/python directory
uv sync --extra dimrset_delivery
# or with pip
pip install --editable .[dimrset_delivery]
```

Once the packages have been successfully installed, you can run the main script (`DimrAutomation.py`). This will prompt you to enter your Deltares username and password. It requires this information to make use of the TeamCity REST API.

## Assumptions

- This script assumes you have a valid Deltares username and password you can use to communicate with the REST APIs.
- This script assumes that you are on the Deltares network (either on location or via VPN).
- This script assumes that you want to automate the release steps for the latest TeamCity DIMR build.
- It also assumes that the previous successful build is the release build for the previous release, as some information has to be looked up about the previous release (such as how many tests failed the last time vs. how many tests failed in this release).

Several helper functions have been created that make use of these APIs in various ways. For example: there is an ExcelHelper that is responsible for gathering all the required information and actually appending that information into an Excel sheet. There is a PublicWikiHelper that is responsible for gathering all information required to update the Public Wiki and for actually updating the PublicWiki.

There is also a settings folder, in which some constants are defined. Most of these settings do not have to be changed as they are simply constants referring to build project identifiers on TeamCity, the id of the main Public Wiki page or the base path on the network to which the DIMRset should be downloaded. The settings also include several lists with the names of the kernels, tools and testcases we expect to find.

Finally, the main class putting everything together is the DimrAutomation class. This class is responsible for orchestrating the automation process by calling all the different helper classes.

## Module Structure

- `helpers/` - Helper modules for various automation tasks
- `lib/` - Library wrappers
- `settings/` - Configuration constants and settings
- `scripts/` - Utility scripts
- `templates/` - HTML templates for email and wiki generation
- `teamcity_test_results.py` - TeamCity test result retrieval script

## Known limitations

- This script has solely been created to automate the release steps for the current DIMR release. It will not work (without changing the code) for any previous DIMR releases. If this ever becomes an issue, the script can be changed to take two additional commandline arguments: one buildnumber of 2. DIMR_collector_release for the desired release and one buildnumber to act as the previous build.
- If there is already a page somewhere else on the wiki with the same name, the script will fail to update the wiki. However, this should not happen in practice.
- If someone has the Excel file that needs to be updated open when you run this script, the Excel file will not be updated. This short-term solution is to print to the console the data that has to be added to the Excel sheet so that it can still be done manually without having to look up all required data first.

## Contact

For questions about the DIMR release process, please contact the DIMR bakkers at DIMRbakkers@deltares.nl.
