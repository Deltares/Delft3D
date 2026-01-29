#!/usr/bin/env python3
"""Assert preconditions are met before the DIMR release process is run."""

import os
import sys
from typing import Optional

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.connection_service_interface import ConnectionServiceInterface
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.step_executer_interface import StepExecutorInterface
from ci_tools.example_utils.logger import LogLevel


class PreconditionsChecker(StepExecutorInterface):
    """
    Checks required preconditions for the DIMR release process.

    This class verifies that all necessary services, connections, and permissions are available
    before running the DIMR automation. Instantiate with a context and services, then call
    `execute_step()` to perform all checks.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.context = context
        self.services = services

    def execute_step(self) -> bool:
        """
        Assert that all preconditions are met before running the DIMR release process.

        Performs checks for service connectivity, network access, and permissions.

        Returns
        -------
        bool
            True if all preconditions are met, False otherwise.

        Raises
        ------
        ValueError
            If any required client is not initialized.
        AssertionError
            If any precondition check fails.
        """
        error_count: int = 0
        self.context.log("Asserting preconditions...")
        try:
            if not self.__are_connections_ok():
                error_count += 1

        except (ValueError, AssertionError) as e:
            self.context.log(f"Preconditions check failed: {str(e)}", severity=LogLevel.ERROR)
            error_count += 1
            raise
        except Exception as e:
            error_count += 1
            raise AssertionError(f"Unexpected error during preconditions check: {e}") from e

        self.context.log("Asserted all preconditions.")
        self.context.log(f"Preconditions check completed and returned {error_count} errors!")
        return error_count == 0

    def __are_connections_ok(self) -> bool:
        """
        Check connections for all required services.

        Raises
        ------
        ValueError
            If required clients are not initialized.
        AssertionError
            If any connection fails.
        """
        self.context.log("Checking connections...")

        try:
            teamcity_ok = self.__check_connection(self.services.teamcity, "TeamCity")
            git_ok = self.__check_connection(
                self.services.git,
                "Git",
            )
            ssh_ok = self.__check_connection(
                self.services.ssh,
                "SSH",
            )
            jira_ok = self.__check_connection(
                self.services.jira,
                "Jira",
            )
            return teamcity_ok and git_ok and ssh_ok and jira_ok
        except Exception as e:
            self.context.log(f"Exception during connection check: {e}", severity=LogLevel.ERROR)
            return False

    def __check_connection(self, client: Optional[ConnectionServiceInterface], name: str) -> bool:
        """
        Check connection for a specific service client.

        Parameters
        ----------
        client : Optional[ConnectionServiceInterface]
            The service client to test.

        name : str
            The name of the service for logging.

        Returns
        -------
        bool
            True if connection is successful, False otherwise.
        """
        if client is None:
            self.context.log(f"{name} client is required but not initialized")
            return False
        self.context.log(f"Testing {name} connection...")
        if not client.test_connection():
            self.context.log(f"Failed to connect to the {name} REST API.", severity=LogLevel.ERROR)
            return False
        self.context.log(f"{name} connection successful")
        return True

    def __is_network_accessible(self) -> bool:
        """
        Check read/write access to the network drive.

        Returns
        -------
        bool
            True if read and write access is available, False otherwise.
        """
        self.context.log(f"Checking read/write access to {self.context.settings.network_base_path}...")

        if self.context.dry_run:
            self.context.log(
                f"Successfully checked for read and write access to {self.context.settings.network_base_path}."
            )
            return True

        try:
            path = self.context.settings.network_base_path
            if os.path.exists(path) and os.access(path, os.R_OK) and os.access(path, os.W_OK):
                self.context.log(
                    f"Successfully checked for read and write access to {self.context.settings.network_base_path}."
                )
                return True
            self.context.log(
                f"Access check failed for {self.context.settings.network_base_path}.", severity=LogLevel.ERROR
            )
            return False
        except OSError as e:
            self.context.log(
                f"Could not access {self.context.settings.network_base_path}: {e}", severity=LogLevel.ERROR
            )
            return False


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args)
        services = Services(context)

        context.log("Starting preconditions check...")
        if PreconditionsChecker(context, services).execute_step():
            context.log("Finished successfully!")
            sys.exit(0)
        else:
            context.log("Preconditions check failed!", severity=LogLevel.ERROR)
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nPreconditions check interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Preconditions check failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
