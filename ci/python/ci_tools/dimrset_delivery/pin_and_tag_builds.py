#!/usr/bin/env python3
"""Pin and tag the appropriate builds in TeamCity."""

import sys

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class PinAndTagger(StepExecutorInterface):
    """
    Executes pinning and tagging of builds in TeamCity.

    This class retrieves artifacts from TeamCity, deploys them via SSH, and performs installation tasks
    as part of the DIMR automation workflow.

    Parameters
    ----------
    services : Services
        Collection of external service clients for artifact retrieval and deployment (e.g., TeamCity, SSH).

    Usage
    -----
    Instantiate with a context and services, then call `execute_step()` to perform the pin and tag operation.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        """
        Initialize the PinAndTagger.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing configuration and state.
        services : Services
            External service client for TeamCity operations.
        """
        self.__context = context
        self.__dry_run = context.dry_run
        self.__dimr_version = context.dimr_version
        self.__build_id = context.build_id
        self.__teamcity_ids = context.settings.teamcity_ids

        self.__teamcity = services.teamcity

    def execute_step(self) -> bool:
        """
        Execute the step to pin and tag builds in TeamCity.

        This method logs the process, checks for required clients, and performs the pinning and tagging
        operations. In dry-run mode, it logs intended actions without making changes.

        Returns
        -------
        bool
            True if the step completed successfully, False otherwise.

        Raises
        ------
        Exception
            If an unexpected error occurs during the process.
        """
        self.__context.log("Pinning and tagging builds...")

        if self.__teamcity is None:
            self.__context.log("TeamCity client is required but not initialized.", severity=LogLevel.ERROR)
            return False

        try:
            self.__pin_and_tag_builds_teamcity()
            self.__context.log("Build pinning and tagging completed successfully!")
            return True
        except Exception as e:
            self.__context.log(f"Error during build tagging: {e}", severity=LogLevel.ERROR)
            return False

    def __pin_and_tag_builds_teamcity(self) -> None:
        """
        Tag all builds and pin the appropriate builds in TeamCity.

        Raises
        ------
        ValueError
            If the TeamCity client is not initialized.
        """
        if self.__teamcity is None:
            raise ValueError("TeamCity client is required but not initialized")

        tag = f"DIMRset_{self.__dimr_version}"
        if self.__dry_run:
            self.__context.log(f"Pin and tag {self.__build_id} and dependent builds with '{tag}' in TeamCity")
            return
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
        context = create_context_from_args(args, require_ssh=False, require_jira=False, require_git=False)
        services = Services(context)

        context.log("Starting pinning and tagging...")
        if PinAndTagger(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed pinning and tagging!", severity=LogLevel.ERROR)
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
