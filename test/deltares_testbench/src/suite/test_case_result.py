"""
Description: Result for test case run
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.utils.comparers.comparison_result import ComparisonResult


class TestCaseResult:
    """Contains the result for a TestCaseConfig"""

    def __init__(self, config: TestCaseConfig) -> None:
        self.__config: TestCaseConfig = config
        self.__results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]] = []

    @property
    def results(self) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        """Result list for all file checks"""
        return self.__results

    @results.setter
    def results(
        self, results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]]
    ):
        self.__results = results

    @property
    def config(self) -> TestCaseConfig:
        """Config that this result belongs to"""
        return self.__config
