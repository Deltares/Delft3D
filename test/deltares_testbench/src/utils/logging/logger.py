"""Default logger implementation.

Copyright (C)  Stichting Deltares, 2024
"""

import os

from src.utils.common import get_default_logging_folder_path
from src.utils.logging.composite_logger import CompositeLogger
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.team_city_logger import TeamCityLogger


class Logger(CompositeLogger):
    def __init__(self, log_level: LogLevel, teamcity: bool) -> None:
        if teamcity:
            console_logger = TeamCityLogger(log_level)
        else:
            console_logger = ConsoleLogger(log_level)

        log_path = self.__get_log_file_path()
        file_logger = FileLogger(log_level, "main_log", log_path)

        super().__init__([console_logger, file_logger])

    def __get_log_file_path(self) -> str:
        current_folder = get_default_logging_folder_path()

        return os.path.join(current_folder, "testbench.log")
