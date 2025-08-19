from abc import ABC, abstractmethod

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext


class ConnectionServiceInterface(ABC):
    """Interface for connection services within the DIMR automation workflow."""

    @abstractmethod
    def __init__(self, username: str, password: str, context: DimrAutomationContext) -> None:
        pass

    @abstractmethod
    def test_connection(self, dry_run: bool) -> bool:
        """
        Test the connection to the service.

        Parameters
        ----------
        dry_run : bool
            Whether to perform a dry run without making actual changes.

        Returns
        -------
        bool
            True if the connection test was successful, False otherwise.
        """
        pass
