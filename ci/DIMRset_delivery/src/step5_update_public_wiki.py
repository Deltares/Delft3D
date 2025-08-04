#!/usr/bin/env python3
"""
DIMR Automation Step 5: Update Public Wiki
This script updates the Public Wiki with the new DIMR release information.
"""

from dimr_context import DimrAutomationContext, parse_common_arguments, create_context_from_args
from helpers.PublicWikiHelper import PublicWikiHelper
from settings.general_settings import DRY_RUN_PREFIX


def update_public_wiki(context: DimrAutomationContext) -> None:
    """Updates the Public Wiki."""
    context.print_status("Updating public wiki...")
    
    # Get required information
    dimr_version = context.get_dimr_version()
    
    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would update public wiki for DIMR version:", dimr_version)
        return
    
    public_wiki = PublicWikiHelper(
        atlassian=context.atlassian,
        teamcity=context.teamcity,
        dimr_version=dimr_version,
    )
    public_wiki.update_public_wiki(context.build_id)
    
    print("Public wiki update completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args)
    
    print("Starting public wiki update...")
    update_public_wiki(context)
    print("Finished")
