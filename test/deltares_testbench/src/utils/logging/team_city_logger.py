"""Logger implementation for teamcity logging.

Copyright (C)  Stichting Deltares, 2024
"""

import logging

from src.utils.common import escape_teamcity
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.teamcity_test_logger import TeamcityTestLogger


class TeamCityLogger(ConsoleLogger):
    def create_test_case_logger(self, test_case_id: str) -> ITestLogger:
        return TeamcityTestLogger(test_case_id, self.log_level)

    def __base_log_message(self, message: str, log_level: int) -> None:
        message_to_log = escape_teamcity(message).strip()
        ConsoleLogger.__base_log_message(self, message_to_log, log_level)

    def __create_console_handler(self, log_level: int) -> logging.Handler:
        format_str = "##teamcity[message text='%(levelname)s : %(message)s']"

        handler = logging.StreamHandler()
        handler.setLevel(log_level)
        handler.setFormatter(logging.Formatter(format_str))

        return handler
