#!/usr/bin/env python3
"""
DIMR Automation Step 2: Download and Install Artifacts
This script downloads the artifacts and installs them on Linux machine.
"""

from dimr_context import DimrAutomationContext, parse_common_arguments, create_context_from_args
from helpers.ArtifactInstallHelper import ArtifactInstallHelper
from settings.general_settings import DRY_RUN_PREFIX


def download_and_install_artifacts(context: DimrAutomationContext) -> None:
    """Downloads the artifacts and installs them on Linux machine."""
    context.print_status("Downloading and installing artifacts...")
    
    # Get required information
    branch_name = context.get_branch_name()
    dimr_version = context.get_dimr_version()
    
    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would download artifacts for build from TeamCity:", context.build_id)
        print(f"{DRY_RUN_PREFIX} Would publish artifacts to network drive")
        print(f"{DRY_RUN_PREFIX} Would publish weekly DIMR via H7")
        return
    
    helper = ArtifactInstallHelper(
        teamcity=context.teamcity,
        ssh_client=context.ssh_client,
        dimr_version=dimr_version,
        branch_name=branch_name,
    )
    helper.publish_artifacts_to_network_drive(context.build_id)
    helper.publish_weekly_dimr_via_h7()
    
    print("Artifacts download and installation completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False)
    
    print("Starting artifact download and installation...")
    download_and_install_artifacts(context)
    print("Finished")
