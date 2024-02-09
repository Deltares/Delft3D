"""
Description: Test Config Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""


# Test case configuration
from typing import List, Optional

from src.config.file_check import FileCheck
from src.config.location import Location
from src.config.program_config import ProgramConfig
from src.config.dependency import Dependency


class TestCaseConfig:
    __test__ = False

    # constructor: initialize variables
    def __init__(self):
        self.__name: str = ""
        self.__version: str = ""
        self.__path: str = ""
        self.__dependency: Optional[Dependency] = None
        self.__locations: List[Location] = []
        self.__shell: Optional[ProgramConfig] = None
        self.__shell_arguments = []
        self.__program_configs = []
        self.__errors: List[str] = []
        self.__checks: List[FileCheck] = []
        self.__max_run_time: float = 0
        self.__ref_run_time: float = -1
        self.__run_time: float = 0
        self.__overrule_ref_max_run_time: bool = False
        self.__absolute_test_case_path: str = ""
        self.__absolute_test_case_reference_path: str = ""
        self.__run_file = ""
        self.__ignore = False
        self.__process_count: int = 1

    @property
    def name(self) -> str:
        """name of the test case"""
        return self.__name

    @name.setter
    def name(self, value: str):
        self.__name = value

    @property
    def version(self) -> str:
        """version of the test case"""
        return self.__version

    @version.setter
    def version(self, value: str):
        self.__version = value

    @property
    def path(self) -> str:
        """relative paths for test case"""
        return self.__path

    @path.setter
    def path(self, value: str):
        self.__path = value

    @property
    def dependency(self) -> Optional[Dependency]:
        """Data that the testcase depends on"""
        return self.__dependency

    @dependency.setter
    def dependency(self, value: Optional[Dependency]):
        self.__dependency = value

    @property
    def locations(self) -> List[Location]:
        """network paths for test case (reference and input)"""
        return self.__locations

    @locations.setter
    def locations(self, value: List[Location]):
        self.__locations = value

    @property
    def absolute_test_case_path(self) -> str:
        """absolute file system path to test case"""
        return self.__absolute_test_case_path

    @absolute_test_case_path.setter
    def absolute_test_case_path(self, value: str):
        self.__absolute_test_case_path = value

    @property
    def absolute_test_case_reference_path(self) -> str:
        """absolute file system path to reference data for test case"""
        return self.__absolute_test_case_reference_path

    @absolute_test_case_reference_path.setter
    def absolute_test_case_reference_path(self, value: str):
        self.__absolute_test_case_reference_path = value

    @property
    def max_run_time(self) -> float:
        """maximum run time of the test case"""
        return self.__max_run_time

    @max_run_time.setter
    def max_run_time(self, value: float):
        self.__max_run_time = value

    @property
    def ref_run_time(self) -> float:
        """maximum run time of the test case as specified in the reference _tb3_char.run file"""
        return self.__ref_run_time

    @ref_run_time.setter
    def ref_run_time(self, value: float):
        self.__ref_run_time = value

    @property
    def run_time(self) -> float:
        """actual run time of the test case"""
        return self.__run_time

    @run_time.setter
    def run_time(self, value: float):
        self.__run_time = value

    @property
    def overrule_ref_max_run_time(self) -> bool:
        """Default: maxRunTime in the config.xml is overruled by maxRunTime in reference/_tb3_char.run
        This can be overruled using this flag
        """
        return self.__overrule_ref_max_run_time

    @overrule_ref_max_run_time.setter
    def overrule_ref_max_run_time(self, value: bool):
        self.__overrule_ref_max_run_time = value

    @property
    def program_configs(self) -> List[ProgramConfig]:
        """programs (list) used in test"""
        return self.__program_configs

    @program_configs.setter
    def program_configs(self, value: List[ProgramConfig]):
        self.__program_configs = value

    @property
    def errors(self) -> List[str]:
        """expected errors in test"""
        return self.__errors

    @errors.setter
    def errors(self, value: List[str]):
        self.__errors = value

    @property
    def shell(self) -> Optional[ProgramConfig]:
        """specific shell for this case configuration"""
        return self.__shell

    @shell.setter
    def shell(self, value: Optional[ProgramConfig]):
        self.__shell = value

    @property
    def shell_arguments(self) -> List[str]:
        """arguments passed to the shell in which the program is running"""
        return self.__shell_arguments

    @property
    def checks(self) -> List[FileCheck]:
        """files to check"""
        return self.__checks

    @property
    def run_file_name(self) -> str:
        """Name (including path) of runfile"""
        return self.__run_file

    @run_file_name.setter
    def run_file_name(self, value: str):
        self.__run_file = value

    @property
    def ignore(self) -> bool:
        """ignore tescase"""
        return self.__ignore

    @ignore.setter
    def ignore(self, value: bool):
        self.__ignore = value

    @property
    def process_count(self) -> int:
        return self.__process_count

    @process_count.setter
    def process_count(self, value: int):
        self.__process_count = value
