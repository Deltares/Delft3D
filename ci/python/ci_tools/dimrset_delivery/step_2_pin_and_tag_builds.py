#!/usr/bin/env python3
"""Pin and tag the appropriate builds in TeamCity."""

import sys

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface


class PinAndTagger(StepExecutorInterface):
    """
    Executes the step to pin and tag builds in TeamCity and Git.

    This class handles the process of retrieving artifacts from TeamCity,
    deploying them to a remote system via SSH, and performing installation
    tasks as part of the DIMR automation workflow.

    services : Services
        The collection of external service clients required for artifact
        retrieval and deployment (e.g., TeamCity, SSH).

    Methods
    -------
    execute_step()
        Downloads artifacts from TeamCity and installs them on the remote Linux machine.
        Supports dry-run mode for testing without performing actual operations.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.__context = context
        self.__dry_run = context.dry_run
        self.__kernel_versions = context.kernel_versions
        self.__dimr_version = context.dimr_version
        self.__build_id = context.build_id
        self.__teamcity_ids = context.settings.teamcity_ids

        self.__git_client = services.git
        self.__teamcity = services.teamcity

    def execute_step(self) -> bool:
        """
        Execute the step to pin and tag builds in TeamCity and Git.

        This method logs the process, checks for required clients (TeamCity and Git),
        and performs the pinning and tagging operations. If running in dry-run mode,
        it logs the actions that would be taken without making changes. On success,
        it logs completion and returns True. On failure, it logs the error and returns False.

        Returns
        -------
            bool: True if the step completed successfully, False otherwise.
        """
        self.__context.log("Pinning and tagging builds...")

        if self.__teamcity is None:
            self.__context.log("TeamCity client is required but not initialized.")
            return False
        if self.__git_client is None:
            self.__context.log("Git client is required but not initialized.")
            return False

        try:
            if self.__dry_run:
                self.__context.log("Pin and tag TC builds")
                self.__context.log(
                    "Would tag git commit with:",
                    f"commit={self.__kernel_versions['build.vcs.number']}, tag=DIMRset_{self.__dimr_version}",
                )
            else:
                self.__pin_and_tag_builds_teamcity()
                self.__git_client.tag_commit(
                    self.__kernel_versions["build.vcs.number"], f"DIMRset_{self.__dimr_version}"
                )
            self.__context.log("Build pinning and tagging completed successfully!")
            return True
        except Exception as e:
            self.__context.log(f"Error during build tagging: {e}")
            return False

    def __pin_and_tag_builds_teamcity(self) -> None:
        """Tag all builds and pin the appropriate builds in TeamCity."""
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")

        tag = f"DIMRset_{self.__dimr_version}"
        self.__teamcity.add_tag_to_build_with_dependencies(self.__build_id, tag=tag)
        # Only pin specific builds
        teamcity_ids_list = list(vars(self.__teamcity_ids).values())
        build_ids_to_pin = self.__teamcity.get_dependent_build_ids_with_filter(self.__build_id, teamcity_ids_list)
        build_ids_to_pin.append(self.__build_id)
        for build_id in build_ids_to_pin:
            self.__teamcity.pin_build(build_id=build_id)


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_atlassian=False, require_ssh=False)
        services = Services(context)

        context.log("Starting pinning and tagging...")
        if PinAndTagger(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed pinning and tagging!")
            sys.exit(1)

    except KeyboardInterrupt:
        print("\npinning and tagging interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"pinning and tagging failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
