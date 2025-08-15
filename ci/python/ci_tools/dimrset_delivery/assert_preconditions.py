#!/usr/bin/env python3
"""Assert preconditions are met before the DIMR release process is run."""

import os
import sys

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)


def _check_api_connections(context: DimrAutomationContext) -> None:
    """Check API connections for TeamCity and Atlassian.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    ValueError
        If required clients are not initialized.
    AssertionError
        If any API connection fails.
    """
    print("Checking API connections...")
    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")

    print("Testing TeamCity API connection...")
    if not context.teamcity.test_api_connection(context.dry_run):
        raise AssertionError("Failed to connect to the TeamCity REST API.")

    print("TeamCity API connection successful")
    if context.atlassian is None:
        raise ValueError("Atlassian client is required but not initialized")

    print("Testing Atlassian API connection...")
    if not context.atlassian.test_api_connection(context.dry_run):
        raise AssertionError("Failed to connect to the Atlassian Confluence REST API.")

    print("Atlassian API connection successful")


def _check_network_access(context: DimrAutomationContext) -> None:
    """Check read/write access to the network drive.

    Parameters
    ----------
    dry_run : bool
        Whether to run in dry-run mode without making actual checks.

    Raises
    ------
    AssertionError
        If network access check fails.
    """
    print("Checking read/write access to the network drive...")
    if context.dry_run:
        print((f"{context.settings.dry_run_prefix} Checking read/write access to {context.settings.network_base_path}"))
        print(f"Successfully checked for read and write access to {context.settings.network_base_path}.")
    else:
        try:
            if not os.path.exists(context.settings.network_base_path):
                raise AssertionError(f"Network path does not exist: {context.settings.network_base_path}")

            if not (
                os.access(context.settings.network_base_path, os.W_OK)
                and os.access(context.settings.network_base_path, os.R_OK)
            ):
                raise AssertionError(f"Insufficient permissions for {context.settings.network_base_path}")

            print(f"Successfully checked for read and write access to {context.settings.network_base_path}.")
        except OSError as e:
            raise AssertionError(f"Could not access {context.settings.network_base_path}: {e}") from e


def _check_ssh_connection(context: DimrAutomationContext) -> None:
    """Check SSH connection to Linux server.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    AssertionError
        If SSH connection fails.
    """
    print("Checking if ssh connection to Linux can be made...")
    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")

    try:
        context.ssh_client.test_connection(context.dry_run)
    except Exception as e:
        raise AssertionError(f"Could not establish ssh connection to {context.settings.linux_address}: {e}") from e


def _check_git_connection(context: DimrAutomationContext) -> None:
    """Check Git connection.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    AssertionError
        If Git connection fails.
    """
    print("Checking if git connection can be made...")
    if context.git_client is None:
        raise ValueError("Git client is required but not initialized")

    try:
        context.git_client.test_connection(context.dry_run)
    except Exception as e:
        raise AssertionError(f"Could not establish git connection: {e}") from e


def assert_preconditions(context: DimrAutomationContext) -> None:
    """Assert that all preconditions are met before the script is fully run.

    This function performs comprehensive checks to ensure all required services,
    connections, and permissions are available before executing the main automation.

    The checks include:
    - API connectivity to TeamCity and Atlassian services
    - Network drive access permissions
    - SSH connectivity to Linux servers
    - Git repository access

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.

    Raises
    ------
    ValueError
        If any required client is not initialized.
    AssertionError
        If any precondition check fails.
    """
    context.log("Asserting preconditions...")
    try:
        _check_api_connections(context)
        _check_network_access(context)
        _check_ssh_connection(context)
        _check_git_connection(context)

        print("Successfully asserted all preconditions.")
        print("Preconditions check completed successfully!")

    except (ValueError, AssertionError) as e:
        print(f"Preconditions check failed: {str(e)}")
        raise
    except Exception as e:
        raise AssertionError(f"Unexpected error during preconditions check: {e}") from e


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args)

        print("Starting preconditions check...")
        assert_preconditions(context)
        print("Finished successfully!")
        sys.exit(0)

    except KeyboardInterrupt:
        print("\nPreconditions check interrupted by user")
        sys.exit(130)  # Standard exit code for keyboard interrupt

    except (ValueError, AssertionError) as e:
        print(f"Preconditions check failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(2)
