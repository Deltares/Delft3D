from abc import ABC, abstractmethod
from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.logging.i_logger import ILogger


class IComparer(ABC):
    """Interface for comparer classes."""

    @abstractmethod
    def compare(
        self,
        left_path: str,
        right_path: str,
        file_check: FileCheck,
        testcase_name: str,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        """Compare contents of left file and right file (usually reference file with generated file).

        Parameters
        ----------
        left_path : str
            Path to the first file.
        right_path : str
            Path to the second file.
        file_check : FileCheck
            File check information.
        testcase_name : str
            Name of the test case.

        Returns
        -------
        List[Tuple[FileCheck, Parameter, ComparisonResult]]
        """
