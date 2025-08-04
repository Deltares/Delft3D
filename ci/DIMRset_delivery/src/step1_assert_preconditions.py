#!/usr/bin/env python3
"""
DIMR Automation Step 1: Assert Preconditions
This script asserts some preconditions are met before the DIMR release process is run.
"""

from dimr_context import DimrAutomationContext, parse_common_arguments, create_context_from_args
from helpers.PreconditionsHelper import PreconditionsHelper


def assert_preconditions(context: DimrAutomationContext) -> None:
    """Asserts some preconditions are met before the script is fully run."""
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
