"""
Description: Local Paths Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""


class Dependency:
    """Class for registering dependencies of tests"""

    def __init__(self, local_dir: str, case_path: str):
        self.__cases_path: str = case_path
        self.__local_dir: str = local_dir

    @property
    def cases_path(self) -> str:
        """Path to the data of the dependent test cases"""
        return self.__cases_path

    @cases_path.setter
    def cases_path(self, value: str):
        self.__cases_path = value

    @property
    def local_dir(self) -> str:
        """Local directory to download the case to"""
        return self.__local_dir

    @local_dir.setter
    def local_dir(self, value: str):
        self.__local_dir = value
