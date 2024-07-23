"""Raw File Content comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import filecmp
import os
from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger


# compare files on content equality
class ContentComparer(IComparer):
    # compare left and right file
    # input: left path, right path, FileCheck instance
    # output: list of (file_check, parameter, ResultComparison) tuples
    def compare(
        self,
        left_path: str,
        right_path: str,
        file_check: FileCheck,
        testcase_name: str,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        logger.debug("Checking: " + file_check.name)

        parameter = Parameter()
        comparison_result = ComparisonResult()

        filename = file_check.name
        left_file_name = os.path.join(left_path, filename)
        right_file_name = os.path.join(right_path, filename)
        try:
            equal = filecmp.cmp(left_file_name, right_file_name)
            if equal:
                comparison_result.passed = True
                comparison_result.result = "OK"
            else:
                comparison_result.passed = False
                comparison_result.maxAbsDiff = 1.0
                comparison_result.maxRelDiff = 1.0
                comparison_result.result = "NOK"
        except Exception:
            comparison_result.passed = False
            comparison_result.error = True
            comparison_result.result = "ERROR"

        return [(testcase_name, file_check, parameter, comparison_result)]
