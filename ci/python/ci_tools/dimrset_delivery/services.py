from typing import Optional

from ci_tools.dimrset_delivery.dimr_context import CredentialEntry, DimrAutomationContext, ServiceName
from ci_tools.dimrset_delivery.lib.git_client import GitClient
from ci_tools.dimrset_delivery.lib.jira import Jira
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity


class Services:
    """
    Stores initialized service clients for automation tasks.

    This class provides access to TeamCity, SSH, and Git clients
    based on the requirements and credentials provided in the context.
    Usage: Instantiate with a DimrAutomationContext to initialize required services.
    """

    jira: Optional[Jira] = None
    teamcity: Optional[TeamCity] = None
    ssh: Optional[SshClient] = None
    git: Optional[GitClient] = None

    def __init__(self, context: DimrAutomationContext) -> None:
        """
        Initialize service clients based on context requirements.

        Parameters
        ----------
        context : DimrAutomationContext
            The context containing requirements and credentials for service initialization.

        Raises
        ------
        ValueError
            If required credentials for a service are missing.
        """
        for service, credential in context.credentials:
            if credential.required:
                self.__setup_service(service, credential, context)

        if self.teamcity:
            context.kernel_versions = self.teamcity.get_kernel_versions()
            context.dimr_version = self.teamcity.get_dimr_version()
            context.branch_name = self.teamcity.get_branch_name()

    def __setup_service(
        self, service_name: ServiceName, entry: CredentialEntry, context: DimrAutomationContext
    ) -> None:
        if not entry.required:
            return

        if service_name == ServiceName.JIRA:
            self.jira = Jira(
                credentials=entry.credential,
                context=context,
            )
        elif service_name == ServiceName.TEAMCITY:
            self.teamcity = TeamCity(
                credentials=entry.credential,
                context=context,
            )
        elif service_name == ServiceName.SSH:
            self.ssh = SshClient(
                credentials=entry.credential,
                context=context,
            )
        elif service_name == ServiceName.GIT:
            self.git = GitClient(
                credentials=entry.credential,
                context=context,
            )
        else:
            raise ValueError(f"Unsupported service type: {service_name.value}")
