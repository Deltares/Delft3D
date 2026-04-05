from abc import ABC, abstractmethod

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext


class ConnectionServiceInterface(ABC):
    """
    Interface for connection services within the DIMR automation workflow.

    This abstract base class defines the required methods for implementing
    connection services that interact with DIMR automation.
    """

    @abstractmethod
    def __init__(self, credentials: Credentials, context: DimrAutomationContext) -> None:
        """
        Initialize the connection service interface.

        Parameters
        ----------
        credentials : Credentials
            Username and Password for authentication.
        context : DimrAutomationContext
            DIMR automation context object.
        """
        pass

    @abstractmethod
    def test_connection(self) -> bool:
        """
        Test the connection to the service.

        Returns
        -------
        bool
            True if the connection test was successful, False otherwise.
        """
        pass
