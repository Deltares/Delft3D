from abc import ABC, abstractmethod

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services


class StepExecutorInterface(ABC):
    """
    Abstract base class for executing steps in the DIMR automation workflow.

    Provides the contract for step executors, requiring initialization with context and services,
    and an execution method.
    """

    @abstractmethod
    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        """
        Initialize the step executor with the given automation context and services.

        Parameters
        ----------
        context : DimrAutomationContext
            The automation context for the step executor.
        services : Services
            The services required for step execution.
        """
        self.context = context
        self.services = services

    @abstractmethod
    def execute_step(self) -> bool:
        """
        Execute the step within the DIMR automation workflow.

        Returns
        -------
        bool
            True if the step execution was successful, False otherwise.
        """
        pass
