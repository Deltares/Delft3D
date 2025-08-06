#!/usr/bin/env python3
"""Assert preconditions are met before the DIMR release process is run."""

from .dimr_context import DimrAutomationContext, create_context_from_args, parse_common_arguments
from .helpers.preconditions_helper import PreconditionsHelper


def assert_preconditions(context: DimrAutomationContext) -> None:
    """Assert some preconditions are met before the script is fully run.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Asserting preconditions...")

    preconditions = PreconditionsHelper(
        atlassian=context.atlassian,
        teamcity=context.teamcity,
        ssh_client=context.ssh_client,
        git_client=context.git_client,
    )
    preconditions.assert_preconditions(context.dry_run)

    print("Preconditions check completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args)

    print("Starting preconditions check...")
    assert_preconditions(context)
    print("Finished")
