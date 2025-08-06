#!/usr/bin/env python3
"""Pin and tag the appropriate builds in TeamCity."""

from .dimr_context import DimrAutomationContext, create_context_from_args, parse_common_arguments
from .helpers.PinHelper import PinHelper
from .settings.general_settings import DRY_RUN_PREFIX


def pin_and_tag_builds(context: DimrAutomationContext) -> None:
    """Pin and tag the appropriate builds."""
    context.print_status("Pinning and tagging builds...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would pin and tag builds in TeamCity for build chain:", context.build_id)
        print(f"{DRY_RUN_PREFIX} Would add tag:", f"DIMRset_{dimr_version}")
        print(
            f"{DRY_RUN_PREFIX} Would tag commit with:",
            f"commit={kernel_versions['build.vcs.number']}, tag=DIMRset_{dimr_version}",
        )
        return

    helper = PinHelper(teamcity=context.teamcity, dimr_version=dimr_version)
    helper.pin_and_tag_builds(context.build_id)

    # Also tag the git commit
    context.git_client.tag_commit(kernel_versions["build.vcs.number"], f"DIMRset_{dimr_version}")

    print("Build pinning and tagging completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_ssh=False)

    print("Starting build pinning and tagging...")
    pin_and_tag_builds(context)
    print("Finished")
