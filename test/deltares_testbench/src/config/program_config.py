"""Program configuration Data Class.

Copyright (C)  Stichting Deltares, 2024
"""

from typing import Dict, List, Optional

from src.config.location import Location


class ProgramConfig:
    """Program configuration."""

    # constructor: always initialize variables
    def __init__(self) -> None:
        self.__shell: Optional[ProgramConfig] = None
        self.__shell_arguments: str = ""
        self.__program_remove_quotes: bool = False
        self.__shell_remove_quotes: bool = False
        self.__ignore_standard_error: bool = False
        self.__ignore_return_value: bool = False
        self.__log_output_to_file: bool = False
        self.__name: str = ""
        self.__case_name: str = ""
        self.__locations: List[Location] = []
        self.__path: str = ""
        self.__search_paths: List[str] = []
        self.__add_search_paths: bool = False
        self.__exclude_search_paths_containing: str = ""
        self.__environment_vars: Dict[str, List[str]] = {}
        self.__environment: Dict[str, str] = {}
        self.__modules: List[str] = []
        self.__absolute_bin_path: str = ""
        self.__working_directory: Optional[str] = None
        self.__arguments: List[str] = []
        self.__sequence: int = 0
        self.__delay: float = 0
        self.__max_run_time: float = 0

    @property
    def shell(self) -> Optional["ProgramConfig"]:
        """Optional shell statement."""
        return self.__shell

    @shell.setter
    def shell(self, value: Optional["ProgramConfig"]) -> None:
        self.__shell = value

    @property
    def program_remove_quotes(self) -> bool:
        """Optional program remove quotes statement."""
        return self.__program_remove_quotes

    @program_remove_quotes.setter
    def program_remove_quotes(self, value: bool) -> None:
        self.__program_remove_quotes = value

    @property
    def shell_remove_quotes(self) -> bool:
        """Optional shell remove quotes statement."""
        return self.__shell_remove_quotes

    @shell_remove_quotes.setter
    def shell_remove_quotes(self, value: bool) -> None:
        self.__shell_remove_quotes = value

    @property
    def ignore_standard_error(self) -> bool:
        """Optional ignore standard error."""
        return self.__ignore_standard_error

    @ignore_standard_error.setter
    def ignore_standard_error(self, value: bool) -> None:
        self.__ignore_standard_error = value

    @property
    def ignore_return_value(self) -> bool:
        """Optional ignore return value."""
        return self.__ignore_return_value

    @ignore_return_value.setter
    def ignore_return_value(self, value: bool) -> None:
        self.__ignore_return_value = value

    @property
    def log_output_to_file(self) -> bool:
        """Optional log output to file."""
        return self.__log_output_to_file

    @log_output_to_file.setter
    def log_output_to_file(self, value: bool) -> None:
        self.__log_output_to_file = value

    @property
    def name(self) -> str:
        """Program name."""
        return self.__name

    @name.setter
    def name(self, value: str) -> None:
        self.__name = value

    @property
    def case_name(self) -> str:
        """Name of the testcase that the program executes."""
        return self.__case_name

    @case_name.setter
    def case_name(self, value: str) -> None:
        self.__case_name = value

    @property
    def locations(self) -> List[Location]:
        """Network paths for program (reference and current)."""
        return self.__locations

    @locations.setter
    def locations(self, value: List[Location]) -> None:
        self.__locations = value

    @property
    def path(self) -> str:
        """Path for program."""
        return self.__path

    @path.setter
    def path(self, value: str) -> None:
        self.__path = value

    @property
    def search_paths(self) -> List[str]:
        """Absolute search paths."""
        return self.__search_paths

    @search_paths.setter
    def search_paths(self, value: List[str]) -> None:
        self.__search_paths = value

    @property
    def add_search_paths(self) -> bool:
        return self.__add_search_paths

    @add_search_paths.setter
    def add_search_paths(self, value: bool) -> None:
        self.__add_search_paths = value

    @property
    def exclude_search_paths_containing(self) -> str:
        return self.__exclude_search_paths_containing

    @exclude_search_paths_containing.setter
    def exclude_search_paths_containing(self, value: str) -> None:
        self.__exclude_search_paths_containing = value

    @property
    def environment_variables(self) -> Dict[str, List[str]]:
        """The environment variables for the program."""
        return self.__environment_vars

    @environment_variables.setter
    def environment_variables(self, value: Dict[str, List[str]]) -> None:
        self.__environment_vars = value

    @property
    def environment(self) -> Dict[str, str]:
        """The operating environment."""
        return self.__environment

    @environment.setter
    def environment(self, value: Dict[str, str]) -> None:
        self.__environment = value

    @property
    def modules(self) -> List[str]:
        """The modules for the environment."""
        return self.__modules

    @property
    def absolute_bin_path(self) -> str:
        """Absolute system path to binary."""
        return self.__absolute_bin_path

    @absolute_bin_path.setter
    def absolute_bin_path(self, value: str) -> None:
        self.__absolute_bin_path = value

    @property
    def working_directory(self) -> Optional[str]:
        """The working directory."""
        return self.__working_directory

    @working_directory.setter
    def working_directory(self, value: Optional[str]) -> None:
        self.__working_directory = value

    @property
    def arguments(self) -> List[str]:
        """Arguments to pass to program."""
        return self.__arguments

    @arguments.setter
    def arguments(self, value: List[str]) -> None:
        self.__arguments = value

    @property
    def sequence(self) -> int:
        """Sequence group identifier to pass to program."""
        return self.__sequence

    @sequence.setter
    def sequence(self, value: int) -> None:
        self.__sequence = value

    @property
    def delay(self) -> float:
        """Given delay before start."""
        return self.__delay

    @delay.setter
    def delay(self, value: float) -> None:
        self.__delay = value

    @property
    def max_run_time(self) -> float:
        """Given maximum runtime."""
        return self.__max_run_time

    @max_run_time.setter
    def max_run_time(self, value: float) -> None:
        self.__max_run_time = value

    @property
    def shell_arguments(self) -> str:
        """Single string of Shell Arguments to be passed on to the shell."""
        return self.__shell_arguments

    @shell_arguments.setter
    def shell_arguments(self, value: str) -> None:
        self.__shell_arguments = value
