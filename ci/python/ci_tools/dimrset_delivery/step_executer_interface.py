from abc import ABC, abstractmethod

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services


class StepExecutorInterface(ABC):
    """
    Interface for executing steps within the DIMR automation workflow.

    This abstract base class defines the contract for step executors, requiring
    implementations to provide initialization with a context and services, and
    an execution method for performing a step.

    Methods
    -------
    __init__(context: DimrAutomationContext, services: Services) -> None
        Initialize the step executor with the given automation context and services.

    execute_step() -> Any
        Execute the step and return the result.
    """

    @abstractmethod
    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
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
