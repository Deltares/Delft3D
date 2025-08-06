#!/usr/bin/env python3
"""Update the Excel sheet with this week's release information."""

from .dimr_context import DimrAutomationContext, create_context_from_args, parse_common_arguments
from .helpers.ExcelHelper import ExcelHelper
from .helpers.SshClient import Direction
from .helpers.TestbankResultParser import TestbankResultParser
from .settings.general_settings import DRY_RUN_PREFIX, LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME
from .settings.teamcity_settings import PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT


def update_excel_sheet(context: DimrAutomationContext) -> None:
    """Update the Excel sheet with this week's release information."""
    context.print_status("Updating Excel sheet...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would update Excel sheet with DIMR version:", dimr_version)
        print(f"{DRY_RUN_PREFIX} Would download Excel from network drive")
        print(f"{DRY_RUN_PREFIX} Would append new row with release information")
        print(f"{DRY_RUN_PREFIX} Would upload updated Excel back to network drive")
        return

    parser = get_testbank_result_parser()
    path_to_excel_file = f"/p/d-hydro/dimrset/{VERSIONS_EXCEL_FILENAME}"

    context.ssh_client.secure_copy(LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.FROM)
    helper = ExcelHelper(
        teamcity=context.teamcity,
        filepath=VERSIONS_EXCEL_FILENAME,
        dimr_version=dimr_version,
        kernel_versions=kernel_versions,
        parser=parser,
    )
    helper.append_row()
    context.ssh_client.secure_copy(LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.TO)

    print("Excel sheet update completed successfully!")


def get_testbank_result_parser() -> TestbankResultParser:
    """Get a new TestbankResultParser for the latest test bench results from a local file."""
    with open(PATH_TO_RELEASE_TEST_RESULTS_ARTIFACT, "rb") as f:
        artifact = f.read()
    return TestbankResultParser(artifact.decode())


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False)

    print("Starting Excel sheet update...")
    update_excel_sheet(context)
    print("Finished")
