#!/usr/bin/env python3
"""Performs Git tagging for DIMRset releases."""

import sys

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class GitTagger(StepExecutorInterface):
    """
    Tags the DIMRset release commit in Git.

    Uses the kernel build VCS number and the DIMR version
    from the automation context to create a Git tag.

    Parameters
    ----------
    services : Services
        Collection of external service clients for artifact retrieval and deployment (e.g., TeamCity, SSH).

    Usage
    -----
    Instantiate with a context and services, then call `execute_step()` to perform the tag operation.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        """
        Initialize the GitTagger.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context containing configuration and state.
        services : Services
            External service client for Git operations.
        """
        self.__context = context
        self.__dry_run = context.dry_run
        self.__kernel_versions = context.kernel_versions
        self.__dimr_version = context.dimr_version

        self.__git_client = services.git
        self.__teamcity = services.teamcity

    def execute_step(self) -> bool:
        """
        Execute the step to tag the DIMRset release in Git.

        This method logs the process, checks for required clients, and performs the tagging
        operation. In dry-run mode, it logs intended actions without making changes.

        Returns
        -------
        bool
            True if the step completed successfully, False otherwise.

        Raises
        ------
        Exception
            If an unexpected error occurs during the process.
        """
        self.__context.log("Tagging builds...")

        if self.__git_client is None:
            self.__context.log("Git client is required but not initialized.", severity=LogLevel.ERROR)
            return False

        if self.__teamcity is None:
            self.__context.log("TeamCity client is required but not initialized.", severity=LogLevel.ERROR)
            return False

        if self.__dry_run:
            self.__context.log(
                f"Would tag commit {self.__kernel_versions['build.vcs.number']} as DIMRset_{self.__dimr_version}"
            )
            return True

        try:
            self.__git_client.tag_commit(self.__kernel_versions["build.vcs.number"], f"DIMRset_{self.__dimr_version}")
            self.__context.log("Tagging completed successfully!")
            return True
        except Exception as e:
            self.__context.log(f"Error during tagging: {e}", severity=LogLevel.ERROR)
            return False


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args, require_ssh=False, require_jira=False)
        services = Services(context)

        context.log("Starting tagging...")
        if GitTagger(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Failed tagging!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\ntagging interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"tagging failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
