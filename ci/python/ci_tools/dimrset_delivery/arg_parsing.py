import argparse

from ci_tools.dimrset_delivery.dimr_context import (
    CredentialEntry,
    Credentials,
    DimrAutomationContext,
    ServiceAuthenticateStore,
    ServiceName,
)


def parse_common_arguments() -> argparse.Namespace:
    """
    Parse common command line arguments for DIMR automation scripts.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.
    """
    parser = argparse.ArgumentParser(description="DIMR Automation Script")

    parser.add_argument("--build_id", type=str, required=True, help="Build ID chain for the DIMR release")
    parser.add_argument(
        "--dry-run", action="store_true", default=False, help="Run in dry-run mode without making any changes"
    )

    parser.add_argument("--jira-username", type=str, default=None, help="Jira username")
    parser.add_argument("--jira-PAT", type=str, default=None, help="Jira Personal Access Token")

    parser.add_argument("--teamcity-username", type=str, default=None, help="TeamCity username")
    parser.add_argument("--teamcity-password", type=str, default=None, help="TeamCity password")

    parser.add_argument("--ssh-username", type=str, default=None, help="SSH username for H7 server")
    parser.add_argument("--ssh-password", type=str, default=None, help="SSH password for H7 server")

    parser.add_argument("--git-username", type=str, default=None, help="Git username")
    parser.add_argument("--git-PAT", type=str, default=None, help="Git Personal Access Token")

    return parser.parse_args()


def create_context_from_args(
    args: argparse.Namespace,
    require_jira: bool = True,
    require_git: bool = True,
    require_teamcity: bool = True,
    require_ssh: bool = True,
) -> DimrAutomationContext:
    """
    Create automation context from parsed arguments.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command line arguments.
    require_jira : bool, optional
        Whether Jira credentials are required. Default is True.
    require_git : bool, optional
        Whether Git credentials are required. Default is True.
    require_teamcity : bool, optional
        Whether TeamCity credentials are required. Default is True.
    require_ssh : bool, optional
        Whether SSH credentials are required. Default is True.

    Returns
    -------
    DimrAutomationContext
        The constructed automation context.
    """
    credentials = ServiceAuthenticateStore()
    credentials.add(
        ServiceName.JIRA,
        CredentialEntry(
            required=require_jira,
            credential=Credentials(username=args.jira_username, password=args.jira_PAT),
        ),
    )
    credentials.add(
        ServiceName.TEAMCITY,
        CredentialEntry(
            required=require_teamcity,
            credential=Credentials(username=args.teamcity_username, password=args.teamcity_password),
        ),
    )
    credentials.add(
        ServiceName.GIT,
        CredentialEntry(
            required=require_git,
            credential=Credentials(username=args.git_username, password=args.git_PAT),
        ),
    )
    credentials.add(
        ServiceName.SSH,
        CredentialEntry(
            required=require_ssh,
            credential=Credentials(username=args.ssh_username, password=args.ssh_password),
        ),
    )

    context = DimrAutomationContext(
        build_id=args.build_id,
        dry_run=args.dry_run,
        credentials=credentials,
        teamcity_logger=True,
    )

    return context
