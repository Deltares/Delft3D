"""
Description: TestCase Failure Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""


class TestCaseFailure(Exception):
    """Custom error for test failures"""

    def __init__(self, value: str):
        self.__value = value

    def __str__(self):
        return repr(self.__value)
